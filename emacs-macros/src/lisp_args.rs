use quote::{quote, TokenStreamExt};
use syn::{export::{TokenStream2, Span}, Ident, Index};

pub fn impl_for_tuples(max_arity: usize) -> TokenStream2 {
    let mut impls = TokenStream2::new();
    for arity in 1..=max_arity {
        impls.append_all(impl_for_tuple(arity));
    }
    impls
}

pub fn impl_for_arrays(max_length: usize) -> TokenStream2 {
    let mut impls = TokenStream2::new();
    for length in 0..=max_length {
        impls.append_all(impl_for_array(length));
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
        values.append_all(quote!(self.#i.into_lisp(env)?.raw, ));
    }

    return quote! {
        unsafe impl<'e, #types> IntoLispArgs<'e> for (#types) where #constraints {
            type LispArgs = [emacs_value; #arity];

            #[inline]
            fn into_lisp_args(self, env: &'e Env) -> Result<Self::LispArgs> {
                Ok([#values])
            }
        }
    };
}

fn impl_for_array(length: usize) -> TokenStream2 {
    let mut values = TokenStream2::new();
    for i in 0..length {
        values.append_all(quote!(self[#i].raw, ));
    }
    // XXX: We can't unify these 2 duplicate implementations, as that would conflict with the
    // implementation for T: IntoLisp. Check this again when specialization lands.
    // https://github.com/rust-lang/rust/issues/31844
    return quote! {
        unsafe impl IntoLispArgs<'_> for &[Value<'_>; #length] {
            type LispArgs = [emacs_value; #length];

            #[inline]
            fn into_lisp_args(self, _: &Env) -> Result<Self::LispArgs> {
                Ok([#values])
            }
        }

        unsafe impl IntoLispArgs<'_> for [Value<'_>; #length] {
            type LispArgs = [emacs_value; #length];

            #[inline]
            fn into_lisp_args(self, _: &Env) -> Result<Self::LispArgs> {
                Ok([#values])
            }
        }
    };
}
