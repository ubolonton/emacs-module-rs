use emacs::{defun, Env, Result, Value, Vector};

emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

fn some_hidden_native_logic() -> bool {
    true
}

// ANCHOR: inc
    /// This docstring will appear in Lisp too!
    #[defun]
    fn inc(x: i64) -> Result<i64> {
        Ok(x + 1)
    }
// ANCHOR_END: inc

// ANCHOR: value
    #[defun]
    fn maybe_call(lambda: Value) -> Result<()> {
        if some_hidden_native_logic() {
            lambda.call([])?;
        }
        Ok(())
    }

    #[defun(user_ptr)]
    fn to_rust_vec_string(input: Vector) -> Result<Vec<String>> {
        let mut vec = vec![];
        for e in input {
            vec.push(e.into_rust()?);
        }
        Ok(vec)
    }
// ANCHOR_END: value

// ANCHOR: env
    // Note that the function takes an owned `String`, not a reference, which would
    // have been understood as a `user-ptr` object containing a Rust string.
    #[defun]
    fn hello(env: &Env, name: String) -> Result<Value<'_>> {
        env.message(format!("Hello, {}!", name))
    }
// ANCHOR_END: env

// ANCHOR: docstring
// `(fn X Y)` is automatically appended, so you don't have to manually do so.
// In help modes, the signature will be (add X Y).

/// Add 2 numbers.
#[defun]
fn add(x: usize, y: usize) -> Result<usize> {
    Ok(x + y)
}
// ANCHOR_END: docstring
