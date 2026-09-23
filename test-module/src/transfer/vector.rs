//! Testing a custom vector (user-ptr).

use emacs::{defun, Env, IntoLisp, Result, Value};

struct Vector {
    pub x: i64,
    pub y: i64,
}

custom_types! {
    Vector;
}

#[defun]
fn swap_components(mut v: Value<'_>) -> Result<Value<'_>> {
    let vec: &mut Vector = unsafe { v.get_mut()? };
    vec.x ^= vec.y;
    vec.y ^= vec.x;
    vec.x ^= vec.y;
    Ok(v)
}

#[defun(user_ptr(direct))]
fn make(x: i64, y: i64) -> Result<Vector> {
    Ok(Vector { x, y })
}

// Same with the above, but manually.
#[defun]
fn make1(x: i64, y: i64) -> Result<Box<Vector>> {
    Ok(Box::new(Vector { x, y }))
}

#[defun]
fn to_list<'e>(env: &'e Env, v: Value<'_>) -> Result<Value<'e>> {
    v.into_rust::<&Vector>()?;
    let v: &Vector = v.into_rust()?;
    let x = v.x.into_lisp(env)?;
    let y = v.y.into_lisp(env)?;
    env.list(&[x, y])
}

#[defun(user_ptr(direct))]
fn add(a: Value<'_>, b: Value<'_>) -> Result<Vector> {
    let a: &Vector = a.into_rust()?;
    let b: &Vector = b.into_rust()?;
    let (x, y) = (b.x + a.x, b.y + a.y);
    Ok(Vector { x, y })
}

#[defun]
fn scale_mutably(times: i64, mut v: Value<'_>) -> Result<()> {
    let v = unsafe { v.get_mut::<Vector>()? };
    v.x *= times;
    v.y *= times;
    Ok(())
}
