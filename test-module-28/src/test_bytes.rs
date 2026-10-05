use emacs::{defun, Env, IntoLisp, Result, Value};

#[defun]
fn bytes_roundtrip(bytes: Vec<u8>) -> Result<Vec<u8>> {
    Ok(bytes)
}

#[defun]
fn bytes_roundtrip_boxed(bytes: Box<[u8]>) -> Result<Box<[u8]>> {
    Ok(bytes)
}

#[defun]
fn bytes_static() -> Result<&'static [u8]> {
    Ok(b"\xff\x00")
}

/// Return a unibyte string of every byte value, in order.
#[defun]
fn bytes_all() -> Result<Vec<u8>> {
    Ok((0..=u8::MAX).collect())
}

/// This tests `IntoLisp` for a byte string literal, which is an array reference, not a slice.
#[defun]
fn bytes_literal(env: &Env) -> Result<Value<'_>> {
    b"\xff\x00".into_lisp(env)
}

#[defun]
fn bytes_borrowed_vec(env: &Env) -> Result<Value<'_>> {
    let bytes = vec![0xff, 0x00];
    (&bytes).into_lisp(env)
}
