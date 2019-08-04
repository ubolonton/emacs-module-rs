use quote::{quote, TokenStreamExt};
use syn::{export::{TokenStream2, Span}, Ident, Index};

pub fn impl_for_tuples(max_arity: usize) -> TokenStream2 {
    let mut impls = TokenStream2::new();
    for arity in 1..=max_arity {
        impls.append_all(impl_for_tuple(arity));
    }
    impls
}

fn impl_for_tuple(arity: usize) -> TokenStream2 {
    let type_vars = (0..arity).map(|n| {
        Ident::new(&format!("T{}", n + 1), Span::call_site())
    });
    let mut types = TokenStream2::new();
    let mut constraints = TokenStream2::new();
    let mut values = TokenStream2::new();
    for (i, var) in type_vars.enumerate() {
        types.append_all(quote!(#var, ));
        constraints.append_all(quote!(#var: IntoLisp<'e>, ));
        let i = Index::from(i);
        values.append_all(quote!(self.#i.into_lisp(env)?.raw,));
    }

    return quote! {
        impl<'e, #types> LispArgs<'e> for (#types) where #constraints {
            type Buffer = [emacs_value; #arity];

            fn into_raw(self, env: &'e Env) -> Result<RawArgs<Self::Buffer>> {
                let mut x = [#values];
                Ok((x.as_mut_ptr(), #arity, x))
            }
        }
    };

//    TokenStream2::new()

//    quote! {
//        impl<'e, T1, T2, T3> LispArgs<'e> for (T1, T2, T3) where T1: IntoLisp<'e>, T2: IntoLisp<'e>, T3: IntoLisp<'e> {
//            type Buffer = [emacs_value; 3];
//
//            fn into_raw(self, env: &'e Env) -> Result<RawArgs<Self::Buffer>> {
//                let mut x = [
//                    self.0.into_lisp(env)?.raw,
//                    self.1.into_lisp(env)?.raw,
//                    self.2.into_lisp(env)?.raw,
//                ];
//                Ok((x.as_mut_ptr(), x.len(), x))
//            }
//        }
//    };
}

