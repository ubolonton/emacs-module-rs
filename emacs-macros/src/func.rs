use std::ops::Range;

use darling::FromMeta;
use quote::{quote, quote_spanned, TokenStreamExt};
use syn::{
    self,
    export::{Span, TokenStream2},
    spanned::Spanned,
    AttributeArgs, FnArg, FnDecl, Ident, ItemFn, Pat,
};

use crate::util::{self, report};

#[derive(Debug)]
enum Arg {
    Env { span: Span },
    Val { span: Span, access: Access, nth: usize, name: Option<Ident> },
}

/// Kinds of argument.
#[derive(Copy, Clone, Debug)]
enum Access {
    Owned,
    Ref,
    RefMut,
}

/// Kinds of `user-ptr` embedding.
#[derive(Debug)]
enum UserPtr {
    /// Embedding through a [`RefCell`]. This is suitable for common use cases, where module
    /// functions can borrow the underlying data back for read/write. It is safe because Lisp
    /// threads are subjected to the GIL. [`BorrowError`]/[`BorrowMutError`] may be signaled at
    /// runtime, depending on how module functions call back into the Lisp runtime.
    ///
    /// [`RefCell`]: https://doc.rust-lang.org/std/cell/struct.RefCell.html
    /// [`BorrowError`]: https://doc.rust-lang.org/std/cell/struct.BorrowError.html
    /// [`BorrowMutError`]: https://doc.rust-lang.org/std/cell/struct.BorrowMutError.html
    RefCell,
    /// Embedding through a [`RwLock`]. Suitable for sharing data between module functions (on Lisp
    /// threads, with [`Env`] access) and pure Rust code (on background threads, without [`Env`]
    /// access).
    ///
    /// [`RwLock`]: https://doc.rust-lang.org/std/sync/struct.RwLock.html
    RwLock,
    /// Embedding through a [`Mutex`]. Suitable for sharing data between module functions (on Lisp
    /// threads, with [`Env`] access) and pure Rust code (on background threads, without [`Env`]
    /// access).
    ///
    /// [`Mutex`]: https://doc.rust-lang.org/std/sync/struct.Mutex.html
    Mutex,
    /// Embedding a `Transfer` value directly. Suitable for immutable data that will only be read
    /// back (not written to) by module functions (writing requires `unsafe` access, and is
    /// discouraged).
    Direct,
}

#[derive(Debug, FromMeta)]
struct FuncOpts {
    /// Name of the function in Lisp, excluding prefix. `None` means sanitized Rust name is used.
    #[darling(default)]
    name: Option<String>,
    /// Whether module path should be used to construct the full Lisp name. `None` means using
    /// crate-wide config.
    #[darling(default)]
    mod_in_name: Option<bool>,
    /// How the return value should be embedded in Lisp as a `user-ptr`. `None` means no embedding.
    #[darling(default)]
    user_ptr: Option<UserPtr>,
}

#[derive(Debug)]
pub struct LispFunc {
    /// The original Rust definition.
    def: ItemFn,
    /// Relevant info about the arguments in Rust.
    args: Vec<Arg>,
    /// Function's arities in Lisp.
    arities: Range<usize>,
    /// Span of the return type. This helps with error reporting.
    output_span: Span,
    opts: FuncOpts,
}

/// We don't use the derived impl provided by darling, since we want a different syntax.
/// See https://github.com/TedDriggs/darling/issues/74.
impl FromMeta for UserPtr {
    fn from_word() -> darling::Result<UserPtr> {
        Ok(UserPtr::RefCell)
    }

    fn from_list(outer: &[syn::NestedMeta]) -> darling::Result<UserPtr> {
        match outer.len() {
            0 => Err(darling::Error::too_few_items(1)),
            1 => {
                let elem = &outer[0];
                match elem {
                    syn::NestedMeta::Meta(syn::Meta::Word(ref ident)) => {
                        match ident.to_string().as_ref() {
                            "refcell" => Ok(UserPtr::RefCell),
                            "mutex" => Ok(UserPtr::Mutex),
                            "rwlock" => Ok(UserPtr::RwLock),
                            "direct" => Ok(UserPtr::Direct),
                            _ => Err(darling::Error::custom("Unknown kind of embedding")
                                .with_span(ident)),
                        }
                    }
                    _ => Err(darling::Error::custom("Expected an identifier").with_span(elem)),
                }
            }
            _ => Err(darling::Error::too_many_items(1)),
        }
    }
}

impl LispFunc {
    pub fn parse(attr_args: AttributeArgs, fn_item: ItemFn) -> Result<Self, TokenStream2> {
        let opts: FuncOpts = match FuncOpts::from_list(&attr_args) {
            Ok(v) => v,
            Err(e) => return Err(e.write_errors()),
        };
        let (args, arities, output_span) = check_signature(&fn_item.decl)?;
        let def = fn_item;
        Ok(Self { def, args, arities, output_span, opts })
    }

    pub fn render(&self) -> TokenStream2 {
        let define_exporter = self.gen_exporter();
        let register_exporter = self.gen_registrator();
        let define_func = &self.def;
        quote! {
            #define_func
            #define_exporter
            #register_exporter
        }
    }

    /// Generates the wrapper function, which decodes input arguments and encodes return value.
    pub fn gen_wrapper(&self) -> TokenStream2 {
        let mut args = TokenStream2::new();
        // Inlined references do not live long enough. We need bindings for them.
        let mut bindings = TokenStream2::new();
        for arg in &self.args {
            match *arg {
                Arg::Env { span } => {
                    // TODO: Find a way not to define inner function, somehow, otherwise the reported
                    // error is confusing (i.e expecting Env, found &Env).
                    args.append_all(quote_spanned!(span=> &**env,))
                }
                Arg::Val { span, access, nth, .. } => {
                    let name = util::arg("arg", nth);
                    // TODO: Create a slice of `emacs_value` once and iterate through it, instead of
                    // using `get_arg`, which creates a slice each call.
                    bindings.append_all(match access {
                        Access::Owned => quote_spanned! {span=>
                            let #name = env.get_arg(#nth).into_rust()?;
                        },
                        // TODO: Support RwLock/Mutex (for the use case of sharing data with
                        // background Rust threads).
                        // TODO: Support direct access.
                        Access::Ref => quote_spanned! {span=>
                            let #name = &*env.get_arg(#nth).into_ref()?;
                        },
                        Access::RefMut => quote_spanned! {span=>
                            let #name = &mut *env.get_arg(#nth).into_ref_mut()?;
                        },
                    });
                    args.append_all(quote_spanned!(span=> #name,));
                }
            }
        }
        let maybe_embed = match &self.opts.user_ptr {
            None => TokenStream2::new(),
            Some(user_ptr) => match user_ptr {
                UserPtr::RefCell => quote_spanned! {self.output_span=>
                    let output = ::std::boxed::Box::new(::std::cell::RefCell::new(output));
                },
                UserPtr::RwLock => quote_spanned! {self.output_span=>
                    let output = ::std::boxed::Box::new(::std::sync::RwLock::new(output));
                },
                UserPtr::Mutex => quote_spanned! {self.output_span=>
                    let output = ::std::boxed::Box::new(::std::sync::Mutex::new(output));
                },
                UserPtr::Direct => quote_spanned! {self.output_span=>
                    let output = ::std::boxed::Box::new(output);
                },
            },
        };
        // XXX: output can be (), but we can't easily know when.
        let into_lisp = quote_spanned! {self.output_span=>
            #[allow(clippy::unit_arg)]
            ::emacs::IntoLisp::into_lisp(output, env)
        };
        let inner = &self.def.ident;
        let wrapper = self.wrapper_ident();
        quote! {
            fn #wrapper(env: &::emacs::CallEnv) -> ::emacs::Result<::emacs::Value<'_>> {
                #bindings
                let output = #inner(#args)?;
                #maybe_embed
                #into_lisp
            }
        }
    }

    /// Generates the exporter function. It will be called in `emacs_module_init` to bind the Lisp
    /// symbol to the defined extern function.
    pub fn gen_exporter(&self) -> TokenStream2 {
        let define_wrapper = self.gen_wrapper();
        let wrapper = self.wrapper_ident();
        let exporter = self.exporter_ident();
        let (min, max) = (self.arities.start, self.arities.end);
        let mut doc = util::doc(&self.def);
        doc.push_str("\n\n");
        doc.push_str(&lisp_signature(&self.args));
        lisp_signature(&self.args);
        let path = match &self.opts.mod_in_name {
            None => {
                let crate_mod_in_name = util::mod_in_name_path();
                quote!({
                    if #crate_mod_in_name.load(::std::sync::atomic::Ordering::Relaxed) {
                        module_path!()
                    } else {
                        ""
                    }
                })
            }
            Some(true) => quote!(module_path!()),
            Some(false) => quote!(""),
        };
        let lisp_name = match &self.opts.name {
            Some(name) => name.clone(),
            None => util::lisp_name(&self.def.ident),
        };
        // TODO: Consider defining `extern "C" fn` directly instead of using export_functions! and
        // CallEnv wrapper.
        quote! {
            #define_wrapper
            fn #exporter(env: &::emacs::Env) -> ::emacs::Result<()> {
                let prefix = ::emacs::globals::lisp_path(#path);
                ::emacs::export_functions! {
                    env, prefix, {
                        #lisp_name => (#wrapper, #min..#max, #doc),
                    }
                }
                Ok(())
            }
        }
    }

    /// Generates the registrator function. It will be called when the shared lib is loaded (by the
    /// OS, before `emacs_module_init` is called by Emacs), to add the exporter to the list of
    /// functions `emacs_module_init` will call (provided that it's generated by
    /// [`#[emacs::module]`]).
    ///
    /// [`#[emacs::module]`]: attr.module.html
    pub fn gen_registrator(&self) -> TokenStream2 {
        let exporter = self.exporter_ident();
        let registrator = self.registrator_ident();
        let init_fns = util::init_fns_path();
        let name = format!("{}", self.def.ident);
        quote! {
            #[::emacs::deps::ctor::ctor]
            fn #registrator() {
                let mut full_path = module_path!().to_owned();
                full_path.push_str("::");
                full_path.push_str(#name);
                let mut funcs = #init_fns.lock()
                    .expect("Failed to acquire a write lock on map of initializers");
                funcs.insert(full_path, ::std::boxed::Box::new(#exporter));
            }
        }
    }

    fn wrapper_ident(&self) -> Ident {
        util::concat("__emr_O_", &self.def.ident)
    }

    fn exporter_ident(&self) -> Ident {
        util::concat("__emrs_E_", &self.def.ident)
    }

    fn registrator_ident(&self) -> Ident {
        util::concat("__emrs_R_", &self.def.ident)
    }
}

fn check_signature(decl: &FnDecl) -> Result<(Vec<Arg>, Range<usize>, Span), TokenStream2> {
    let mut i: usize = 0;
    let mut err = TokenStream2::new();
    let mut has_env = false;
    let mut args: Vec<Arg> = vec![];
    let errors = &mut err;
    for fn_arg in &decl.inputs {
        match fn_arg {
            FnArg::Captured(capt) => {
                let ty = &capt.ty;
                let span = fn_arg.span();
                args.push(if is_env(ty) {
                    match ty {
                        syn::Type::Reference(_) => (),
                        _ => report(errors, fn_arg, "Can only take an &Env, not an Env"),
                    }
                    if has_env {
                        report(errors, fn_arg, "&Env must be passed only once")
                    }
                    has_env = true;
                    Arg::Env { span }
                } else {
                    let access = match ty {
                        syn::Type::Reference(syn::TypeReference { mutability, .. }) => {
                            match mutability {
                                Some(_) => Access::RefMut,
                                None => Access::Ref,
                            }
                        }
                        _ => Access::Owned,
                    };
                    let name = match &capt.pat {
                        Pat::Ident(pat_ident) => Some(pat_ident.ident.clone()),
                        Pat::Wild(_) => None,
                        _ => {
                            report(errors, &capt.pat, "Expected identifier");
                            continue;
                        }
                    };
                    let a = Arg::Val { span, access, name, nth: i };
                    i += 1;
                    a
                });
            }
            FnArg::SelfRef(_) => report(errors, fn_arg, "Cannot take &self argument"),
            FnArg::SelfValue(_) => report(errors, fn_arg, "Cannot take self argument"),
            // TODO: Support this.
            FnArg::Ignored(_) => report(errors, fn_arg, "Ignored argument is not supported"),
            FnArg::Inferred(_) => {
                report(errors, fn_arg, "Argument with inferred type is not supported")
            }
        }
    }
    // TODO: Make the Span span the whole return type.
    let output_span = match &decl.output {
        syn::ReturnType::Type(_, ty) => ty.span(),
        _ => {
            report(errors, &decl.fn_token, "Must return emacs::Result<T> where T: IntoLisp<'_>");
            decl.fn_token.span()
        }
    };
    if err.is_empty() {
        Ok((args, Range { start: i, end: i }, output_span))
    } else {
        Err(err)
    }
}

// XXX
fn is_env(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(syn::TypeReference { elem, .. }) => is_env(elem),
        syn::Type::Path(syn::TypePath { qself: None, ref path }) => {
            let str_path = format!("{}", quote!(#path));
            str_path.ends_with("Env")
        }
        _ => false,
    }
}

fn lisp_name(arg: &Arg) -> Option<String> {
    match arg {
        Arg::Env { .. } => None,
        Arg::Val { name: None, .. } => Some("_".to_owned()),
        Arg::Val { name: Some(ident), .. } => Some(util::lisp_name(ident).to_uppercase()),
    }
}

fn lisp_signature(args: &[Arg]) -> String {
    let mut sig = "(fn".to_owned();
    for arg in args.iter().flat_map(lisp_name) {
        sig.push_str(" ");
        sig.push_str(&arg);
    }
    sig.push_str(")");
    sig
}
