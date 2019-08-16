use darling::{self, FromMeta};
use quote::quote;
use syn::{export::TokenStream2, AttributeArgs, ItemFn};

use crate::util;

#[derive(Debug)]
enum Name {
    /// Use crate's name.
    Crate,
    /// Use initializer function's name.
    Fn,
    /// Explicitly specify a name.
    Str(String),
}

#[derive(Debug, FromMeta)]
struct ModuleOpts {
    /// Name of this module (feature name).
    #[darling(default)]
    name: Name,
    /// Separator following the feature name to form the prefix in functions' full Lisp name.
    #[darling(default = "default::separator")]
    separator: String,
    /// Whether module path should be used to construct functions' full Lisp name.
    #[darling(default = "default::mod_in_name")]
    mod_in_name: bool,
    /// Alternative prefix (instead of the feature name) to use in functions' full Lisp name.
    #[darling(default)]
    defun_prefix: Option<String>,
}

#[derive(Debug)]
pub struct Module {
    def: ItemFn,
    opts: ModuleOpts,
}

mod default {
    pub fn separator() -> String {
        "-".into()
    }
    pub fn mod_in_name() -> bool {
        true
    }
    impl Default for super::Name {
        fn default() -> Self {
            super::Name::Crate
        }
    }
}

/// We don't use the derived impl provided by darling, since we want a different syntax.
/// See https://github.com/TedDriggs/darling/issues/74.
impl FromMeta for Name {
    // Use syn::, because it should have been the (non-existent) reexported version from `darling`.
    fn from_list(outer: &[syn::NestedMeta]) -> darling::Result<Name> {
        match outer.len() {
            0 => Err(darling::Error::too_few_items(1)),
            1 => {
                let elem = &outer[0];
                match elem {
                    syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                        match path.segments.last().unwrap().ident.to_string().as_ref() {
                            "fn" => Ok(Name::Fn),
                            "crate" => Ok(Name::Crate),
                            _ => Err(darling::Error::custom("Expected crate/fn").with_span(path)),
                        }
                    }
                    syn::NestedMeta::Lit(syn::Lit::Str(lit)) => Ok(Name::Str(lit.value())),
                    _ => {
                        Err(darling::Error::custom("Expected crate/fn or a string").with_span(elem))
                    }
                }
            }
            _ => Err(darling::Error::too_many_items(1)),
        }
    }

    fn from_string(lit: &str) -> darling::Result<Name> {
        Ok(Name::Str(lit.to_owned()))
    }
}

impl Module {
    pub fn parse(attr_args: AttributeArgs, fn_item: ItemFn) -> Result<Self, TokenStream2> {
        let opts: ModuleOpts = match ModuleOpts::from_list(&attr_args) {
            Ok(v) => v,
            Err(e) => return Err(e.write_errors()),
        };
        Ok(Self { opts, def: fn_item })
    }

    pub fn render(&self) -> TokenStream2 {
        let define_init = self.gen_init();
        let register_init = Self::gen_registrator();
        let define_hook = &self.def;
        quote! {
            #define_hook
            #define_init
            #register_init
        }
    }

    pub fn gen_registrator() -> TokenStream2 {
        let init = Self::init_ident();
        quote! {
            ::emacs::__module_init!(#init);
        }
    }

    pub fn gen_init(&self) -> TokenStream2 {
        let init = Self::init_ident();
        let separator = &self.opts.separator;
        let hook = &self.def.sig.ident;
        let init_fns = util::init_fns_path();
        let prefix = util::prefix_path();
        let mod_in_name = util::mod_in_name_path();
        let crate_mod_in_name = &self.opts.mod_in_name;
        let feature = match &self.opts.name {
            Name::Crate => quote!(::emacs::init::lisp_pkg(module_path!())),
            Name::Str(name) => quote!(#name.to_owned()),
            Name::Fn => {
                let name = util::lisp_name(hook);
                quote!(#name.to_owned())
            }
        };
        let defun_prefix = match &self.opts.defun_prefix {
            None => quote!(feature.clone()),
            Some(defun_prefix) => quote!(#defun_prefix.to_owned()),
        };
        let set_prefix = quote! {
            {
                let mut prefix = #prefix.try_lock()
                    .expect("Failed to acquire write lock on module prefix");
                *prefix = [#defun_prefix, #separator.to_owned()];
            }
        };
        let configure_mod_in_name = quote! {
            #mod_in_name.store(#crate_mod_in_name, ::std::sync::atomic::Ordering::Relaxed);
        };
        let export_lisp_funcs = quote! {
            {
                let funcs = #init_fns.try_lock()
                    .expect("Failed to acquire a read lock on map of initializers");
                for (name, func) in funcs.iter() {
                    func(env)?
                }
            }
        };
        quote! {
            #[allow(non_snake_case)]
            fn #init(env: &::emacs::Env) -> ::emacs::Result<::emacs::Value<'_>> {
                let feature = #feature;
                #set_prefix
                #configure_mod_in_name
                #export_lisp_funcs
                #hook(env)?;
                env.provide(&feature)
            }
        }
    }

    fn init_ident() -> TokenStream2 {
        quote!(__emrs_auto_init__)
    }
}
