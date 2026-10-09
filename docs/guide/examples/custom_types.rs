// ANCHOR: hash_map
use std::collections::HashMap;
use emacs::{defun, Env, Result};

#[emacs::module(name = "rs-hash-map", separator = "/")]
fn init(env: &Env) -> Result<()> {
    type Map = HashMap<String, String>;

    #[defun(user_ptr)]
    fn make() -> Result<Map> {
        Ok(Map::new())
    }

    #[defun]
    fn get(map: &Map, key: String) -> Result<Option<&String>> {
        Ok(map.get(&key))
    }

    #[defun]
    fn set(map: &mut Map, key: String, value: String) -> Result<Option<String>> {
        Ok(map.insert(key, value))
    }

    Ok(())
}
// ANCHOR_END: hash_map

emacs::plugin_is_GPL_compatible!();
