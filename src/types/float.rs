use super::*;

impl FromLisp<'_> for f64 {
    fn from_lisp(value: Value<'_>) -> Result<Self> {
        unsafe_raw_call!(value.env, extract_float, value.raw)
    }
}

impl IntoLisp<'_> for f64 {
    fn into_lisp(self, env: &Env) -> Result<Value<'_>> {
        unsafe_raw_call_value!(env, make_float, self)
    }
}
