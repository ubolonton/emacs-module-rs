# Embedding Rust values in Lisp

If a type implements `Transfer`, its `Box`-wrapped values can be moved into Lisp, to be owned by the GC.

Lisp code sees these as opaque "embedded user pointers" (whose printed representations look like`#<user-ptr ...>`). For these values to be usable, a Rust module needs to export additional functions to manipulate them.

Since these values are owned by the GC, Rust code can only safely access them through immutable references. To make them useful, interior mutability is usually needed. Therefore `Transfer` is implemented for `RefCell`, `Mutex`, and `RwLock`. Note that currently, only `RefCell` is useful, since a GC-integrated equivalent of `Arc` has not been implemented.

For example, a module that allows Emacs to use Rust's `HashMap` may look like this:

```rust
use std::cell::RefCell;
use std::collections::HashMap;
use emacs::{defun, Env, Result, Value};

#[emacs::module(name = "rs-hash-map", separator = "/")]
fn init(env: &Env) -> Result<Value<'_>> {
    type Map = RefCell<HashMap<String, String>>;

    #[defun]
    fn make() -> Result<Map> {
        Ok(RefCell::new(HashMap::new()))
    }

    #[defun]
    fn get<'e>(env: &'e Env, map: &Map, key: String) -> Result<Value<'e>> {
        map.borrow().get(&key).into_lisp(env)
    }

    #[defun]
    fn set(map: &Map, key: String, value: String) -> Result<Option<String>> {
        Ok(map.borrow_mut().insert(key,value))
    }
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
