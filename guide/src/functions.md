# Writing Functions

You can use the attribute macro `#[defun]` to export Rust functions to the Lisp runtime, so that Lisp code can call them. The exporting process happens when the module is loaded, even if the definitions are inside another function that is never called, or inside a private module.

## Input Parameters

Each parameter must be one of the following:
- An owned value of a type that implements `FromLisp`. This is for simple data types that have an equivalent in Lisp.
    ```rust
    /// This docstring will appear in Lisp too!
    #[defun]
    fn inc(x: i64) -> Result<i64> {
        Ok(x + 1)
    }
    ```
- A shared/mutable reference. This gives access to data structures that other module functions have created and embedded in the Lisp runtime (through `user-ptr` objects).
    ```rust
    #[defun]
    fn stash_pop(repo: &mut git2::Repository) -> Result<()> {
        repo.stash_pop(0, None)?;
        Ok(())
    }
    ```
- A Lisp `Value`, or one of its "sub-types" (e.g. `Vector`). This allows holding off the conversion to Rust data structures until necessary, or working with values that don't have a meaningful representation in Rust, like Lisp lambdas.
    ```rust
    #[defun]
    fn maybe_call(lambda: Value) -> Result<()> {
        if some_hidden_native_logic() {
            lambda.call([])?;
        }
        Ok(())
    }

    #[defun(user_ptr)]
    fn to_rust_vec_string(input: Vector) -> Result<Vec<String>> {
        let mut vec = vec![];
        for e in input {
            vec.push(e.into_rust()?);
        }
        Ok(vec)
    }
    ```
- An `&Env`. This enables interaction with the Lisp runtime. It does not appear in the function's Lisp signature. This is unnecessary if there is already another parameter with type `Value`, which allows accessing the runtime through `Value.env`.
    ```rust
    // Note that the function takes an owned `String`, not a reference, which would
    // have been understood as a `user-ptr` object containing a Rust string.
    #[defun]
    fn hello(env: &Env, name: String) -> Result<Value<'_>> {
        env.message(format!("Hello, {}!", name))
    }
    ```

## Return Value

The return type must be `Result<T>`, where `T` is one of the following:
- A type that implements `IntoLisp`. This is for simple data types that have an equivalent in Lisp.
    ```rust
    /// Return the path to the .git dir.
    /// Return `nil' if the given path is not in a repo,
    /// or if the .git path is not valid utf-8.
    #[defun]
    fn dot_git_path(path: String) -> Result<Option<String>> {
        Ok(git2::Repository::discover(&path).ok().and_then(|repo| {
            repo.path().to_str().map(|s| s.to_owned())
        }))
    }
    ```
- An arbitrary type. This allows embedding a native data structure in a `user-ptr` object, for read-write use cases. It requires `user_ptr` option to be specified. If the data is to be shared with background Rust threads, `user_ptr(rwlock)` or `user_ptr(mutex)` must be used instead.
    ```rust
    #[defun(user_ptr)]
    fn repo(path: String) -> Result<git2::Repository> {
        Ok(git2::Repository::discover(&path)?)
    }
    ```
- A type that implements `Transfer`. This allows embedding a native data structure in a `user-ptr` object, for read-only use cases. It requires `user_ptr(direct)` option to be specified.
- `Value`, or one of its "sub-types" (e.g. `Vector`). This is mostly useful for returning an input parameter unchanged.

See [Custom Types](./custom-types.md) for more details on embedding Rust data structures in Lisp's `user-ptr` objects.

## Naming

By default, the function's Lisp name has the form `<feature-prefix>[mod-prefix]<base-name>`.
- `feature-prefix` is the feature name followed by `-`. This can be customized by the `name`, `defun_prefix`, and `separator` [options](./module.md#options) on `#[emacs::module]`.
- `mod-prefix` is constructed from the function's Rust module path (with `_` and `::` replaced by `-`). This can be turned off crate-wide, or for individual function, using the option `mod_in_name`.
- `base-name` is the function's Rust name (with `_` replaced by `-`). This can be overridden with the option `name`.

Examples:

```rust
// Assuming crate's name is `native_parallelism`.

#[emacs::module(separator = "/")]
fn init(_: &Env) -> Result<()> { Ok(()) }

mod shared_state {
    mod thread {
        // Ignore the nested mod's.
        // (native-parallelism/make-thread "name")
        #[defun(mod_in_name = false)]
        fn make_thread(name: String) -> Result<Value<'_>> {
            ..
        }
    }

    mod process {
        // (native-parallelism/shared-state-process-launch "bckgrnd")
        #[defun]
        fn launch(name: String) -> Result<Value<'_>> {
            ..
        }

        // Specify a name explicitly, since Rust identifier cannot contain `:`.
        // (native-parallelism/process:pool "http-client" 2 8)
        #[defun(mod_in_name = false, name = "process:pool")]
        fn pool(name: String, min: i64, max: i64) -> Result<Value<'_>> {
            ..
        }
    }
}
```

## Documentation

`#[defun]` converts Rust's docstring into Lisp's docstring. It also automatically constructs and appends the [function's signature](https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Documentation.html#Function-Documentation) to the end of the docstring, so that help modes can correctly display it.

```rust
// `(fn X Y)` is automatically appended, so you don't have to manually do so.
// In help modes, the signature will be (add X Y).

/// Add 2 numbers.
#[defun]
fn add(x: usize, y: usize) -> Result<usize> {
    Ok(x + y)
}
```
