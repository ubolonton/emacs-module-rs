//! Testing RefCell embedded in user-ptr.

use emacs::{defun, Result, Value};
use std::cell::RefCell;

// TODO: Add tests for Mutex and RwLock, and more tests for RefCell.

/// Wrap the given integer in a RefCell.
#[defun(user_ptr)]
fn wrap(x: i64) -> Result<i64> {
    Ok(x)
}

#[defun]
fn unwrap(r: Value<'_>) -> Result<i64> {
    let r: &RefCell<i64> = r.into_rust()?;
    Ok(*r.try_borrow()?)
}

/// Mutably increment the wrapped integer, returning the new value.
#[defun]
fn inc(x: &mut i64) -> Result<i64> {
    *x += 1;
    Ok(*x)
}

/// Unwrap the integer, call the given function while still holding the reference.
#[defun]
#[allow(clippy::trivially_copy_pass_by_ref)] // TODO: Test with sth else not i64.
fn unwrap_and_call(_: &i64, lambda: Value<'_>) -> Result<()> {
    lambda.env.call_flex("funcall", lambda)?;
    Ok(())
}
