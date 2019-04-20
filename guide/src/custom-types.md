# Embedding Rust Values in Lisp

Speeding up Emacs is one of the goals of dynamic modules. Too many back-and-forth conversions between Rust's data structures and Lisp's can defeat the purpose. The solution to this is embedding Rust data structures in opaque `user-ptr` Lisp objects.

If a type implements `Transfer`, its heap-allocated (`Box`-wrapped) values can be moved into the Lisp runtime, where the GC will become its owner.

Lisp code sees these as opaque "embedded user pointers", whose printed representation is something like `#<user-ptr ptr=0x102e10b60 finalizer=0x103c9c390>`. For these values to be useful, a Rust module needs to export additional functions to manipulate them.

Since these values are owned by the GC, Rust code can only safely access them through immutable references. Therefore, interior mutability is usually needed. As a result, `Transfer` is implemented for the smart pointer types `RefCell`, `Mutex`, and `RwLock`.

To return an embedded value, a function needs to be exported with a `user_ptr` option:
- `user_ptr`: Embedding through a `RefCell`. This is suitable for common use cases, where module functions can borrow the underlying data back for read/write. It is safe because Lisp threads are subjected to the GIL. `BorrowError`/`BorrowMutError` may be signaled at runtime, depending on how module functions call back into the Lisp runtime.
- `user_ptr(rwlock)`, `user_ptr(mutex)`: Embedding through a `RwLock`/`Mutex`. This is suitable for sharing data between module functions (on Lisp threads, with `Env` access) and pure Rust code (on background threads, without access to an `Env`).
- `user_ptr(direct)`: Embedding a `Transfer` value directly. This is suitable for immutable data that will only be read back (not written to) by module functions (writing requires `unsafe` access, and is discouraged).

As an example, a module that allows Emacs to use Rust's `HashMap` may look like this:

```rust
use std::collections::HashMap;
use emacs::{defun, Env, Result, Value};

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
        Ok(map.insert(key,value))
    }

    Ok(())
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

**Notes**:
- `Value.into_rust()` has a runtime type check, which fails with the error `'rust-wrong-type-user-ptr` if the value is a `user-ptr` object of a different type.
- Input parameters with reference types are interpreted as `RefCell`-embedded `user-ptr` objects. For other kinds of embedding, you will have to use a `Value` parameter, and acquire the reference manually, since locking strategy (including deadlock avoidance/detection) should be module-specific.
    ```rust
    use std::sync::RwLock;

    #[defun(user_ptr(rwlock))]
    fn make() -> Result<Map> {
        Ok(Map::new())
    }

    #[defun]
    fn process(v: Value<'_>) -> Result<Value<'_>> {
        let lock: &RwLock<Map> = v.into_rust()?;
        let map = lock.try_read().map_err(|_| failure::err_msg("map is busy"))?;
        Ok(map.get(&key).into_lisp(v.env)?)
    }
    ```
