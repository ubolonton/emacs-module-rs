use emacs::{defun, Result, Value};
use std::cell::RefCell;

// TODO: Add tests for Mutex and RwLock, and more tests for RefCell.

/// Wrap the given integer in a RefCell.
#[defun]
fn wrap(x: i64) -> Result<RefCell<i64>> {
    Ok(RefCell::new(x))
}

#[defun]
fn unwrap(r: Value<'_>) -> Result<i64> {
    let r: &RefCell<i64> = r.into_rust()?;
    Ok(*r.try_borrow()?)
}

/// Mutably increment the wrapped integer, returning the new value.
#[defun]
fn inc(r: Value<'_>) -> Result<i64> {
    let r: &RefCell<i64> = r.into_rust()?;
    let mut x = r.try_borrow_mut()?;
    *x += 1;
    Ok(*x)
}

/// Unwrap the integer, call the given function while still holding the reference.
#[defun]
fn unwrap_and_call(r: Value<'_>, lambda: Value<'_>) -> Result<()> {
    let env = r.env;
    let r: &RefCell<i64> = r.into_rust()?;
    let x = r.try_borrow()?;
    env.call("funcall", &[lambda])?;
    Ok(())
}
