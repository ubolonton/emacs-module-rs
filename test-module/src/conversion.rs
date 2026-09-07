//! Testing type conversion between Lisp and Rust.

use emacs::{defun, Env, IntoLisp, Result, Value, Vector};

emacs::use_symbols! { nil }

#[defun]
fn to_lowercase_or_nil(env: &Env, input: Option<String>) -> Result<Value<'_>> {
    let output = input.map(|s| s.to_lowercase());
    // This tests IntoLisp for Option<&str>.
    output.as_ref().into_lisp(env)
}

#[defun]
fn identity_i8(i: i8) -> Result<i8> {
    Ok(i)
}

#[defun]
fn identity_u8(i: u8) -> Result<u8> {
    Ok(i)
}

#[defun]
fn u64_overflow() -> Result<u64> {
    Ok(u64::max_value())
}

#[defun]
fn copy_string_contents(v: Value, size: usize) -> Result<String> {
    let mut buffer = vec![0u8; size];
    let s = v.copy_string_contents(&mut buffer)?;
    Ok(String::from_utf8_lossy(s).to_string())
}

#[defun]
fn string_to_bytes(v: Value) -> Result<Vector> {
    let env = v.env;
    let bytes = v.clone_string_contents()?;
    let n = bytes.len();
    let result = env.make_vector(n, nil)?;
    for i in 0..n {
        result.set(i, bytes[i])?;
    }
    Ok(result)
}

// Bindings for vector functions (vec_get, vec_set, vec_size).

#[defun]
fn vec_size(v: Vector) -> Result<usize> {
    Ok(v.len())
}

#[defun]
fn vec_get(v: Vector, i: i64) -> Result<Value> {
    v.get(i as usize)
}

#[defun]
fn vec_set(v: Vector, i: i64, value: Value) -> Result<()> {
    v.set(i as usize, value)
}

#[defun]
fn identity_if_vector(v: Vector) -> Result<Vector> {
    Ok(v)
}

#[defun]
fn stringify_num_vector(v: Vector) -> Result<Vector> {
    for i in 0..v.len() {
        let x: i64 = v.get(i)?;
        v.set(i, format!("{}", x))?;
    }
    Ok(v)
}

#[defun]
fn make_vector(length: usize, init: Value) -> Result<Vector> {
    init.env.make_vector(length, init)
}
