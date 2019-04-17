use emacs::{defun, Result};
use std::cell::RefCell;
use std::collections::HashMap;

type Map = HashMap<String, String>;

#[defun]
fn make() -> Result<RefCell<Map>> {
    Ok(RefCell::new(HashMap::new()))
}

#[defun]
fn get(map: &Map, key: String) -> Result<Option<&String>> {
    Ok(map.get(&key))
}

#[defun]
fn set(map: &mut Map, key: String, value: String) -> Result<Option<String>> {
    Ok(map.insert(key, value))
}
