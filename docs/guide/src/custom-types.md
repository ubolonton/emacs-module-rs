# Embedding Rust Values in Lisp

Speeding up Emacs is one of the goals of dynamic modules. Too many back-and-forth conversions between Rust's data structures and Lisp's can defeat the purpose. The solution to this is embedding Rust data structures in opaque `user-ptr` Lisp objects.

If a type implements `Transfer`, its heap-allocated (`Box`-wrapped) values can be moved into the Lisp runtime, where the GC will become its owner.

Lisp code sees these as opaque "embedded user pointers", whose printed representation is something like `#<user-ptr ptr=0x102e10b60 finalizer=0x103c9c390>`. For these values to be useful, a Rust module needs to export additional functions to manipulate them.

Since these values are owned by the GC, Rust code can only safely access them through immutable references. Therefore, interior mutability is usually needed. As a result, `Transfer` is implemented for the smart pointer types `RefCell`, `Mutex`, `RwLock`, `Rc`, and `Arc`.

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

```lisp
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
    fn get(v: Value<'_>, key: String) -> Result<Value<'_>> {
        let lock: &RwLock<Map> = v.into_rust()?;
        let map = lock.try_read().map_err(|_| Error::msg("map is busy"))?;
        map.get(&key).into_lisp(v.env)
    }
    ```

## Lifetime-constrained Types

When a type is constrained by a (non-static) lifetime, its value cannot be embedded unchanged. Before embedding, the lifetime must be **soundly** elided. In other words, static ownership must be correctly given up.

The typical example is a struct holding a reference to another struct:

```rust
pub struct Tree;

pub struct Node<'t> {
    pub tree: &'t Tree,
}

impl Tree {
    pub fn root_node(&self) -> Node<'_> {
        ...
    }
}

impl<'t> Node<'t> {
    pub fn child(&self) -> Node<'t> {
        ...
    }
}
```

In this case, the lifetime can be elided by turning the static reference into a dynamic ref-counted pointer. The [rental crate](https://github.com/jpernst/rental) provides a convenient way to do this:

```rust
#[macro_use]
extern crate rental;

use std::{rc::Rc, marker::PhantomData};
use emacs::{defun, Result};

// PhantomData is need because map_suffix requires a type parameter.
// See https://github.com/jpernst/rental/issues/35.
pub struct PhantomNode<'t, T>(Node<'t>, PhantomData<T>);

impl<'t> PhantomNode<'t, ()> {
    fn child(&self) -> Self {
        PhantomNode(self.0.child(), PhantomData)
    }
}

rental! {
    pub mod inner {
        use std::rc::Rc;

        // Self-referential struct that holds both
        // the actual Node and the ref-counted Tree.
        #[rental(map_suffix = "T")]
        pub struct RentingNode<T: 'static> {
            tree: Rc<super::Tree>,
            node: super::PhantomNode<'tree, T>
        }
    }
}

type RentingNode = inner::RentingNode<()>;

#[defun(user_ptr)]
fn root_node(tree: Value) -> Result<RentingNode> {
    let rc: &Rc<Tree> = tree.into_rust()?;
    Ok(RentingNode::new(rc.clone(), |tree| tree.root_node()))
}

#[defun(user_ptr)]
fn child(node: &RentingNode) -> Result<RentingNode> {
    node.map(|n| n.child())
}
```

Note that there's no `unsafe` involved directly, as the soundness proofs are already encapsulated in `rental` macros.
