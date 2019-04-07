use quote::quote;
use syn::{
    AttributeArgs, export::TokenStream2,
    ItemFn,
};

use crate::util::{self, report};

#[derive(Debug)]
pub enum Name {
    Crate,
    Str(String)
}

#[derive(Debug)]
pub struct Module {
    feature: Name,
    separator: String,
    def: ItemFn,
}

impl Module {
    // TODO XXX FIX: Figure out how to do this with `darling`. It must already have a way, right?
    // See https://github.com/TedDriggs/darling/issues/74.
    pub fn parse(attr_args: AttributeArgs, fn_item: ItemFn) -> Result<Self, TokenStream2> {
        let mut module = Self {
            feature: Name::Crate,
            separator: "-".to_owned(),
            def: fn_item,
        };
        let mut err = TokenStream2::new();
        let errors = &mut err;
        let mut name_configured = false;
        for attr_arg in &attr_args {
            match attr_arg {
                syn::NestedMeta::Literal(l) => {
                    report(errors, l, "Literal options are not supported")
                }
                syn::NestedMeta::Meta(m) => {
                    match m {
                        syn::Meta::Word(i) => {
                            report(errors, i, "Unknown option")
                        }
                        syn::Meta::NameValue(syn::MetaNameValue { ident, lit, .. }) => {
                            match format!("{}", ident).as_str() {
                                "name" => {
                                    match lit {
                                        syn::Lit::Str(name) => {
                                            if name_configured {
                                                report(errors, m, "Name was already specified");
                                            } else {
                                                name_configured = true;
                                                module.feature = Name::Str(name.value());
                                            }
                                        }
                                        _ => report(errors, lit, "Expected string")
                                    }
                                }
                                "separator" => {
                                    match lit {
                                        syn::Lit::Str(separator) => {
                                            module.separator = separator.value();
                                        }
                                        _ => report(errors, lit, "Expected string")
                                    }
                                }
                                _ => report(errors, ident, "Unknown option")
                            }
                        }
                        syn::Meta::List(syn::MetaList { ident , nested, .. }) => {
                            match format!("{}", ident).as_str() {
                                "name" => {
                                    let mut i =  0;
                                    for m in nested {
                                        i += 1;
                                        if i > 1 {
                                            report(errors, m, "Too many args");
                                            continue;
                                        }
                                        match m {
                                            syn::NestedMeta::Meta(syn::Meta::Word(source)) => {
                                                match format!("{}", source).as_str() {
                                                    "fn" => {
                                                        if name_configured {
                                                            report(errors, m, "Name was already specified");
                                                        } else {
                                                            name_configured = true;
                                                            module.feature = Name::Str(util::lisp_name(&module.def.ident));
                                                        }
                                                    }
                                                    "crate" => {
                                                        if name_configured {
                                                            report(errors, m, "Name was already specified");
                                                        } else {
                                                            name_configured = true;
                                                            module.feature = Name::Crate;
                                                        }
                                                    }
                                                    _ => report(errors, source, "Expected crate/fn")
                                                }
                                            }
                                            _ => {
                                                report(errors, nested, "Expected crate/fn")
                                            }
                                        }
                                    }
                                    if i == 0 {
                                        report(errors, m, "Expected name(crate) or name(fn)");
                                    }
                                }
                                _ => report(errors, ident, "Unknown option")
                            }
                        }
                    }
                }
            }
        }
        if !err.is_empty() {
            return Err(err);
        }
        Ok(module)
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
        let feature = quote!(feature);
        let separator = &self.separator;
        let hook = &self.def.ident;
        let init_fns = util::init_fns_path();
        let prefix = util::prefix_path();
        let set_feature = match &self.feature {
            Name::Str(name) => quote! {
                let #feature = #name.to_owned();
            },
            Name::Crate => quote! {
                let #feature = ::emacs::globals::lisp_pkg(module_path!());
            },
        };
        let set_prefix = quote! {
            {
                let mut prefix = #prefix.try_lock()
                    .expect("Failed to acquire write lock on module prefix");
                *prefix = [#feature.clone(), #separator.to_owned()];
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
                #set_feature
                #set_prefix
                #export_lisp_funcs
                #hook(#env)?;
                #env.provide(&#feature)
            }
        }
    }

    fn init_ident() -> TokenStream2 {
        quote!(__emrs_auto_init__)
    }
}
