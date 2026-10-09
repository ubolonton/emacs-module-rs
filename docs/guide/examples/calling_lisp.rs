use emacs::{defun, Env, IntoLisp, Result, Value, Vector};

emacs::plugin_is_GPL_compatible!();

#[emacs::module]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

fn env_methods(env: &Env) -> Result<()> {
// ANCHOR: env_methods
env.intern("defun")?;

env.message("Hello")?;

env.type_of(5.into_lisp(env)?)?;

env.provide("my-module")?;

env.list((1, "str", true))?;
// ANCHOR_END: env_methods
Ok(())
}

fn call_by_name(env: &Env) -> Result<()> {
// ANCHOR: call_by_name
// (list "str" 2)
env.call("list", ("str", 2))?;
// ANCHOR_END: call_by_name
Ok(())
}

fn call_value(env: &Env) -> Result<()> {
// ANCHOR: call_value
let list = env.intern("list")?;
// (symbol-function 'list)
let subr = env.call("symbol-function", [list])?;
// (funcall 'list "str" 2)
env.call(list, ("str", 2))?;
// (funcall (symbol-function 'list) "str" 2)
env.call(subr, ("str", 2))?;
subr.call(("str", 2))?; // Like the above, but shorter.
// ANCHOR_END: call_value
Ok(())
}

fn add_hook(env: &Env) -> Result<()> {
// ANCHOR: add_hook
// (add-hook 'text-mode-hook 'variable-pitch-mode)
env.call("add-hook", [
    env.intern("text-mode-hook")?,
    env.intern("variable-pitch-mode")?,
])?;
// ANCHOR_END: add_hook
Ok(())
}

// ANCHOR: listify_vec
#[defun]
fn listify_vec(vector: Vector) -> Result<Value> {
    let mut args = vec![];
    for e in vector {
        args.push(e)
    }
    vector.value().env.call("list", &args)
}
// ANCHOR_END: listify_vec

mod symbols {
// ANCHOR: use_symbols
use emacs::{defun, use_symbols, Result, Value};

use_symbols! {
    left right center
}

#[defun(mod_in_name = false)]
fn classify(pos: Value<'_>) -> Result<String> {
    if pos == *left {
        Ok("left".to_owned())
    } else if pos == *right {
        Ok("right".to_owned())
    } else if pos == *center {
        Ok("center".to_owned())
    } else {
        Ok("unknown".to_owned())
    }
}
// ANCHOR_END: use_symbols
}

mod renamed_symbols {
use emacs::use_symbols;

// ANCHOR: use_symbols_rename
use_symbols! {
    nil t
    buffer_read_only => "buffer-read-only"
}
// ANCHOR_END: use_symbols_rename
}

mod functions {
// ANCHOR: use_functions
use emacs::{defun, use_functions, Env, Result};

use_functions! {
    message
    string_to_number => "string-to-number"
}

#[defun]
fn greet_parsed(env: &Env, s: String) -> Result<()> {
    let n: i64 = env.call(string_to_number, (s,))?.into_rust()?;
    env.call(message, (format!("Got {}", n),))?;
    Ok(())
}
// ANCHOR_END: use_functions
}
