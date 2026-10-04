use emacs::{defun, Bytes, Result};

#[defun]
fn bytes_roundtrip(bytes: Bytes) -> Result<Bytes> {
    Ok(bytes)
}

#[defun]
fn bytes_static() -> Result<Bytes<&'static [u8]>> {
    Ok(Bytes(b"\xff\x00"))
}

/// Return a unibyte string of every byte value, in order.
#[defun]
fn bytes_all() -> Result<Bytes<Vec<u8>>> {
    Ok(Bytes((0..=u8::MAX).collect()))
}
