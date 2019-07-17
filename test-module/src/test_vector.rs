//! Testing bindings for vector functions (vec_get, vec_set, vec_size).

use emacs::{defun, Result, Value, Vector};

#[defun(mod_in_name = false)]
fn vec_size(v: Vector) -> Result<i64> {
    v.size().map(|s| s as i64)
}

#[defun(mod_in_name = false)]
fn vec_get(v: Vector, i: i64) -> Result<Value> {
    v.get(i as usize)
}

#[defun(mod_in_name = false)]
fn vec_set(v: Vector, i: i64, value: Value) -> Result<()> {
    v.set(i as usize, value)
}

#[defun(mod_in_name = false)]
fn identity_if_vector(v: Vector) -> Result<Vector> {
    Ok(v)
}
