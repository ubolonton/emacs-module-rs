use emacs::{defun, Result};
use std::cell::RefCell;

// TODO: Add tests for Mutex and RwLock, and more tests for RefCell.

/// Wrap the given integer in a RefCell.
#[defun]
fn make(x: i64) -> Result<RefCell<i64>> {
    Ok(RefCell::new(x))
}

#[defun]
fn mutate_twice(r: &RefCell<i64>) -> Result<()> {
    let mut x = r.try_borrow_mut()?;
    let mut y = r.try_borrow_mut()?;
    *x = 1;
    *y = 2;
    Ok(())
}