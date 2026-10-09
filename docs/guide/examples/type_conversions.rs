use emacs::{defun, Env, IntoLisp, Result, Value};

emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

fn into_rust(value: Value<'_>) -> Result<()> {
// ANCHOR: into_rust
let i: i64 = value.into_rust()?; // error if Lisp value is not an integer
let f: f64 = value.into_rust()?; // error if Lisp value is nil

let s = value.into_rust::<String>()?;
let s: Option<String> = value.into_rust()?; // None if Lisp value is nil
let b: Vec<u8> = value.into_rust()?; // raw bytes of a Lisp string
// ANCHOR_END: into_rust
Ok(())
}

fn into_lisp(env: &Env) -> Result<()> {
// ANCHOR: into_lisp
"abc".into_lisp(env)?;
"a\0bc".into_lisp(env)?;
b"\xff\0".into_lisp(env)?; // unibyte string, needs feature emacs-28

5.into_lisp(env)?;
65.3.into_lisp(env)?;

().into_lisp(env)?; // nil
true.into_lisp(env)?; // t
false.into_lisp(env)?; // nil
// ANCHOR_END: into_lisp
Ok(())
}

// ANCHOR: xor_bytes
// (xor-bytes "\377\0" 1) returns "\376\1". Returning `Vec<u8>` needs feature emacs-28.
#[defun]
fn xor_bytes(data: Vec<u8>, key: u8) -> Result<Vec<u8>> {
    Ok(data.iter().map(|b| b ^ key).collect())
}
// ANCHOR_END: xor_bytes

fn equality(env: &Env) -> Result<()> {
// ANCHOR: equality
// Two references to the same interned symbol are eq.
let a = env.intern("hello")?;
let b = env.intern("hello")?;
assert!(a == b);

// Two separately allocated strings with the same content are not eq.
let s1 = "hi".into_lisp(env)?;
let s2 = "hi".into_lisp(env)?;
assert!(s1 != s2);
// ANCHOR_END: equality
Ok(())
}

mod global_equality {
use emacs::{defun, Result, Value};

// ANCHOR: global_equality
use emacs::use_symbols;

use_symbols! { nil }

#[defun]
fn is_nil(v: Value<'_>) -> Result<bool> {
    Ok(v == *nil)
}
// ANCHOR_END: global_equality
}

fn vectors(env: &Env) -> Result<()> {
// ANCHOR: vectors
env.make_vector(5, ())?;

env.vector((1, "x", true))?;
// ANCHOR_END: vectors
Ok(())
}
