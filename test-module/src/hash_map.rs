use std::cell::RefCell;
use std::collections::HashMap;
use emacs::{defun, Result, Value, Env, IntoLisp, CallEnv};

type Map = RefCell<HashMap<String, String>>;

#[defun]
fn make() -> Result<Map> {
    Ok(RefCell::new(HashMap::new()))
}

// XXX: Bad ergonomics.
#[defun]
fn get<'e>(env: &'e Env, map: &Map, key: String) -> Result<Value<'e>> {
    map.borrow().get(&key).into_lisp(env)
}

// XXX: Inefficient.
#[defun]
fn get0(map: &Map, key: String) -> Result<Option<String>> {
    Ok(map.borrow().get(&key).map(|s| s.to_owned()))
}

// TODO: Standardize on wrapping with RefCell, and generate code like below.
#[allow(unused)]
fn wrapper(env: &CallEnv) -> Result<Value<'_>> {
    type Map = HashMap<String, String>;

    // User code works with the inner type, instead of RefCell.
    fn inner(map: &Map, key: String) -> Result<Option<&String>> {
        Ok(map.get(&key))
    }

    // Generate additional borrowing code when user func uses reference type.
    let map = {
        let ref_cell: &RefCell<Map> = env.parse_arg(0)?;
        &*ref_cell.try_borrow()?
    };
    let key: String = env.parse_arg(1)?;
    let output = inner(map, key)?;
    output.into_lisp(env)
}

#[defun]
fn set(map: &Map, key: String, value: String) -> Result<Option<String>> {
    Ok(map.borrow_mut().insert(key, value))
}
