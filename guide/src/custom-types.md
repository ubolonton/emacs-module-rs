# Embedding Rust values in Lisp

If a type implements `Transfer`, its `Box`-wrapped values can be moved into Lisp, to be owned by the GC.

Lisp code sees these as opaque "embedded user pointers" (whose printed representations look like`#<user-ptr ...>`). For these values to be usable, a Rust module needs to export additional functions to manipulate them.

Since these values are owned by the GC, Rust code can only safely access them through immutable references. To make them useful, interior mutability is usually needed. Therefore `Transfer` is implemented for `RefCell`, `Mutex`, and `RwLock`. Note that currently, only `RefCell` is useful, since a GC-integrated equivalent of `Arc` has not been implemented.

For example, a module that allows Emacs to use Rust's `HashMap` may look like this:

```rust
use std::cell::RefCell;
use std::collections::HashMap;
use emacs;

fn init(env: &Env) -> Result<Value<'_>> {
    type Map = RefCell<HashMap<String, String>>;

    fn make(_: &CallEnv) -> Result<Map> {
        Ok(RefCell::new(HashMap::new()))
    }

    fn get(env: &CallEnv) -> Result<Value<'_>> {
        let map: &Map = env.parse_arg(0)?;
        let key: String = env.parse_arg(1)?;
        map.borrow().get(&key).into_lisp(env)
    }

    fn set(env: &CallEnv) -> Result<Option<String>> {
        let map: &Map = env.parse_arg(0)?;
        let key: String = env.parse_arg(1)?;
        let value: String = env.parse_arg(2)?;
        Ok(map.borrow_mut().insert(key,value))
    }

    emacs::emacs_export_functions! {
        env, "rs-hash-map/", {
            "make" => (make, 0..0),
            "get"  => (get,  2..2),
            "set"  => (set,  3..3),
        }
    }

    env.provide("rs-hash-map")
}
```

```emacs-lisp
(let ((m (rs-hash-map/make)))
  (rs-hash-map/get m "a")     ; -> nil

  (rs-hash-map/set m "a" "1") ; -> nil
  (rs-hash-map/get m "a")     ; -> "1"

  (rs-hash-map/set m "a" "2") ; -> "1"
  (rs-hash-map/get m "a"))    ; -> "2"
```
