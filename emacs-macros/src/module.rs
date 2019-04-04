use darling::FromMeta;
use quote::quote;
use syn::{
    AttributeArgs, export::TokenStream2,
    ItemFn,
};

use crate::util;

#[derive(Debug, FromMeta)]
struct ModuleOpts {
    #[darling(default)]
    provide: Option<String>,
    #[darling(default)]
    prefix: Option<String>,
}

#[derive(Debug)]
pub struct Module {
    feature: String,
    prefix: String,
    def: ItemFn,
}

impl Module {
    pub fn parse(attr_args: AttributeArgs, fn_item: ItemFn) -> Result<Self, TokenStream2> {
        let opts: ModuleOpts = match ModuleOpts::from_list(&attr_args) {
            Ok(v) => v,
            Err(e) => return Err(e.write_errors()),
        };
        // TODO: Type-check this.
        let def = fn_item;
        let feature = opts.provide.unwrap_or_else(|| util::lisp_name(&def.ident));
        let prefix = opts.prefix.unwrap_or_else(|| format!("{}-", feature));
        Ok(Self { feature, prefix, def })
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
            ::emacs::emacs_module_init!(#init);
        }
    }

    pub fn gen_init(&self) -> TokenStream2 {
        let env = quote!(env);
        let init = Self::init_ident();
        let feature = &self.feature;
        let hook = &self.def.ident;
        let init_fns = util::init_fns_path();
        let prefix = util::prefix_path();
        let prefix_value = &self.prefix;
        let set_prefix = quote! {
            {
                let mut prefix = #prefix.lock()
                    .expect("Failed to acquire write lock on module prefix");
                *prefix = #prefix_value;
            }
        };
        let export_lisp_funcs = quote! {
            {
                let funcs = #init_fns.lock()
                    .expect("Failed to acquire a read lock on map of initializers");
                for (name, func) in funcs.iter() {
                    func(#env)?
                }
            }
        };
        quote! {
            #[allow(non_snake_case)]
            fn #init(#env: &::emacs::Env) -> ::emacs::Result<::emacs::Value<'_>> {
                #set_prefix
                #export_lisp_funcs
                #hook(#env)?;
                #env.provide(#feature)
            }
        }
    }

    fn init_ident() -> TokenStream2 {
        quote!(__emrs_auto_init__)
    }
}
