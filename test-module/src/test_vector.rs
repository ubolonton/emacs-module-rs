//! Testing bindings for vector functions (vec_get, vec_set, vec_size).

use emacs::{defun, Result, Value, Vector};

#[defun(mod_in_name = false)]
fn vec_size(v: Vector) -> Result<usize> {
    Ok(v.len())
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

#[defun(mod_in_name = false)]
fn stringify_num_vector(v: Vector) -> Result<Vector> {
    for i in 0..v.len() {
        let x: i64 = v.get(i)?;
        v.set(i, format!("{}", x))?;
    }
    Ok(v)
}

#[defun(mod_in_name = false)]
fn make_vector(length: usize, init: Value) -> Result<Vector> {
    init.env.make_vector(length, init)
}