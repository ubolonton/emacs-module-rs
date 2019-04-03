use proc_macro::TokenStream;
use std::ops::Range;

use quote::{quote, quote_spanned, TokenStreamExt};
use syn::{
    self, AttributeArgs,
    export::{Span, TokenStream2}, FnArg, FnDecl, Ident, ItemFn,
};
use syn::spanned::Spanned;

use crate::util::{concat, lisp_name, report, doc};

#[derive(Copy, Clone, Debug)]
pub enum Arg {
    Env { span: Span },
    Val { span: Span, nth: usize },
}

#[derive(Debug)]
pub struct LispFunc {
    name: String,
    ident: Ident,
    args: Vec<Arg>,
    arities: Range<usize>,
    doc: String,
    output_span: Span,
}

#[derive(FromMeta)]
pub struct FuncOpts {
    #[darling(default)]
    name: Option<String>
}

fn check_signature(decl: &FnDecl) -> (Vec<Arg>, Range<usize>, Span, TokenStream2) {
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
                    let a = Arg::Val { span, nth: i };
                    i += 1;
                    a
                });
            }
            FnArg::SelfRef(_) => report(errors, fn_arg, "Cannot take &self argument"),
            FnArg::SelfValue(_) => report(errors, fn_arg, "Cannot take self argument"),
            // TODO: Support this.
            FnArg::Ignored(_) => report(errors, fn_arg, "Ignored argument is not supported"),
            FnArg::Inferred(_) => report(errors, fn_arg, "Argument with inferred type is not supported"),
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
    (args, Range { start: i, end: i }, output_span, err)
}

pub fn parse(_attr_args: AttributeArgs, fn_item: ItemFn) -> (LispFunc, TokenStream2) {
    let (args, arities, output_span, errors) = check_signature(&fn_item.decl);
    let doc = doc(&fn_item);
    let ident = fn_item.ident;
    let name = lisp_name(&ident);
    (LispFunc { name, ident, args, arities, doc, output_span }, errors)
}

pub fn gen_wrapper(func: &LispFunc, errors: TokenStream2) -> (Ident, TokenStream2) {
    let mut args = TokenStream2::new();
    for arg in &func.args {
        match *arg {
            Arg::Env { span } => {
                // TODO: Find a way not to define inner function, somehow, otherwise the reported
                // error is confusing (i.e expecting Env, found &Env).
                args.append_all(quote_spanned!(span=> &**env,))
            }
            Arg::Val { span, nth } => {
                args.append_all(quote_spanned!(span=> env.parse_arg(#nth)?,))
            }
        }
    }
    let into_lisp = quote_spanned!(func.output_span=> .into_lisp(env));
    let inner = &func.ident;
    let wrapper = concat("_emrs_O_", inner);
    let define_wrapper = quote! {
        #errors
        fn #wrapper(env: &::emacs::CallEnv) -> ::emacs::Result<::emacs::Value<'_>> {
            #inner(#args)?
            #into_lisp
        }
    };
    (wrapper, define_wrapper)
}

pub fn gen_exporter(func: &LispFunc, errors: TokenStream2) -> (Ident, TokenStream2) {
    let (wrapper, define_wrapper) = gen_wrapper(&func, errors);
    let lisp_name = &func.name;
    let exporter = concat("_emrs_E_", &func.ident);
    let (min, max) = (func.arities.start, func.arities.end);
    let doc = &func.doc;
    // TODO: Consider defining `extern "C" fn` directly instead of using emacs_export_functions!.
    let defs = quote! {
        #define_wrapper
        fn #exporter(env: &::emacs::Env) -> ::emacs::Result<()> {
            ::emacs::emacs_export_functions! {
                env, crate::__EMACS_MODULE_RS_PREFIX__, {
                    #lisp_name => (#wrapper, #min..#max, #doc),
                }
            }
            Ok(())
        }
    };
    (exporter, defs)
}

pub fn gen_registrator(func: &LispFunc, exporter: Ident) -> TokenStream2 {
    let lisp_name = &func.name;
    let registrator = concat("_emrs_R_", &func.ident);
    quote! {
        #[::emacs::deps::ctor::ctor]
        fn #registrator() {
            let mut funcs = ::emacs::func::__EMACS_MODULE_RS_AUTO_FUNCS__.lock()
                .expect("Fail to acquire a lock on map of exporters");
            funcs.insert(#lisp_name.to_owned(), ::std::boxed::Box::new(#exporter));
        }
    }
}

// XXX
fn is_env(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(syn::TypeReference { elem, .. }) => {
            is_env(elem)
        }
        syn::Type::Path(syn::TypePath { qself: None, ref path }) => {
            let str_path = format!("{}", quote!(#path));
            str_path.ends_with("Env")
        }
        _ => false,
    }
}
