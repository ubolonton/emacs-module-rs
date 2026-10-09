//! Testing a custom hash map: HashMap embedded in user-ptr.

use emacs::{defun, Result};
use std::cell::RefCell;
use std::collections::HashMap;

type Map = HashMap<String, String>;

#[defun(user_ptr)]
fn make() -> Result<Map> {
    Ok(Map::new())
}

// Same with the above, but manually.
#[defun]
fn make1() -> Result<RefCell<Map>> {
    Ok(RefCell::new(Map::new()))
}

// Same with the above, but even more manually.
#[defun]
fn make2() -> Result<Box<RefCell<Map>>> {
    Ok(Box::new(RefCell::new(Map::new())))
}

#[defun]
fn get(map: &Map, key: String) -> Result<Option<&String>> {
    Ok(map.get(&key))
}

#[defun]
fn set(map: &mut Map, key: String, value: String) -> Result<Option<String>> {
    Ok(map.insert(key, value))
}
