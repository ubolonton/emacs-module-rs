//! Testing the source locations that `#[defun]` records.

use emacs::{defun, Result};

/// A docstring, then a non-macro attribute, both above `#[defun]`.
#[allow(clippy::unnecessary_wraps)]
#[defun]
fn attributes_above() -> Result<()> {
    Ok(())
}

#[defun]
fn no_attributes() -> Result<()> {
    Ok(())
}
