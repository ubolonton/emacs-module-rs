use std::collections::HashMap;
use emacs::{defun, Env, Error, IntoLisp, Result, Value};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "rs-hash-map", separator = "/")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

type Map = HashMap<String, String>;

// ANCHOR: rwlock
    use std::sync::RwLock;

    #[defun(user_ptr(rwlock))]
    fn make() -> Result<Map> {
        Ok(Map::new())
    }

    #[defun]
    fn get(v: Value<'_>, key: String) -> Result<Value<'_>> {
        let lock: &RwLock<Map> = v.into_rust()?;
        let map = lock.try_read().map_err(|_| Error::msg("map is busy"))?;
        map.get(&key).into_lisp(v.env)
    }
// ANCHOR_END: rwlock
