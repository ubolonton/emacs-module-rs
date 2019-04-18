use std::ops::Range;

use darling::FromMeta;
use quote::{quote, quote_spanned, TokenStreamExt};
use syn::{
    self,
    export::{Span, TokenStream2},
    spanned::Spanned,
    AttributeArgs, FnArg, FnDecl, Ident, ItemFn,
};

use crate::util::{self, report};

#[derive(Copy, Clone, Debug)]
enum Arg {
    Env { span: Span },
    Val { span: Span, access: Access, nth: usize },
}

#[derive(Copy, Clone, Debug)]
enum Access {
    Owned,
    Ref,
    RefMut,
}

#[derive(FromMeta)]
struct FuncOpts {
    #[darling(default)]
    name: Option<String>,
    #[darling(default)]
    mod_in_name: Option<bool>,
}

#[derive(Debug)]
pub struct LispFunc {
    /// Name of the function in Lisp, excluding prefix.
    name: String,
    /// The original Rust definition.
    def: ItemFn,
    /// Relevant info about the arguments in Rust.
    args: Vec<Arg>,
    /// Function's arities in Lisp.
    arities: Range<usize>,
    /// Span of the return type. This helps with error reporting.
    output_span: Span,
    /// Whether module path should be used to construct the full Lisp name. None means using
    /// crate-wide config.
    mod_in_name: Option<bool>,
}

impl LispFunc {
    pub fn parse(attr_args: AttributeArgs, fn_item: ItemFn) -> Result<Self, TokenStream2> {
        let opts: FuncOpts = match FuncOpts::from_list(&attr_args) {
            Ok(v) => v,
            Err(e) => return Err(e.write_errors()),
        };
        let (args, arities, output_span) = check_signature(&fn_item.decl)?;
        let def = fn_item;
        let name = opts.name.unwrap_or_else(|| util::lisp_name(&def.ident));
        let mod_in_name = opts.mod_in_name;
        Ok(Self { name, def, args, arities, output_span, mod_in_name })
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
                Arg::Val { span, access, nth } => {
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
        // XXX: result can be (), but we can't easily know when.
        let into_lisp = quote_spanned! {self.output_span=>
            #[allow(clippy::unit_arg)]
            ::emacs::IntoLisp::into_lisp(result, env)
        };
        let inner = &self.def.ident;
        let wrapper = self.wrapper_ident();
        quote! {
            fn #wrapper(env: &::emacs::CallEnv) -> ::emacs::Result<::emacs::Value<'_>> {
                #bindings
                let result = #inner(#args)?;
                #into_lisp
            }
        }
    }

    /// Generates the exporter function. It will be called in `emacs_module_init` to bind the Lisp
    /// symbol to the defined extern function.
    pub fn gen_exporter(&self) -> TokenStream2 {
        let define_wrapper = self.gen_wrapper();
        let wrapper = self.wrapper_ident();
        let lisp_name = &self.name;
        let exporter = self.exporter_ident();
        let (min, max) = (self.arities.start, self.arities.end);
        let doc = util::doc(&self.def);
        let path = match &self.mod_in_name {
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
                    let a = Arg::Val { span, access, nth: i };
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
