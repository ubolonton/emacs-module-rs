# Writing Functions
You can write Rust functions to be called by Lisp code. These functions must have the signature `fn(&CallEnv) -> Result<T>`, where `T` is any type that implements the `IntoLisp` trait:

```rust
fn inc(env: &CallEnv) -> Result<i64> {
    let x: i64 = env.parse_arg(0)?;
    Ok(x + 1)
}

fn sum(env: &CallEnv) -> Result<Value<'_>> {
    let x: f64 = env.parse_arg(0)?;
    let y: f64 = env.parse_arg(1)?;
    (x + y).into_lisp(env)
}
```

## Getting arguments

To get a typed argument, use `env.parse_arg(i)`:

```rust
let path: String = env.parse_arg(0)?;
let count: i64 = env.parse_arg(1)?;
```

To get an argument as `Value`, use `env.get_arg(i)`. This is useful in case you want to use it as a return value or delay the conversion to Rust until necessary:

```rust
fn get_or_default(env: &CallEnv) -> Result<Value<'_>> {
    let map: &HashMap<String, String> = env.parse_arg(0)?;
    let key: &str = env.parse_arg(1)?;
    match m.get(key) {
        Some(v) => Ok(v.into_lisp(env)?),
        None => env.get_arg(2),
    }
}
```

To get all arguments as a `Vec<Value<'_>>`, use `env.args()`.

## Exporting named functions

To allow Lisp code to call these functions by name, use `emacs_export_funtions!`:

```rust
fn inc(env: &CallEnv) -> Result<i64> {
    env.parse_arg::<i64>(0)? + 1
}

fn dec(env: &CallEnv) -> Result<i64> {
    env.parse_arg::<i64>(0)? - 1
}

// This should be put inside the init function.
emacs::emacs_export_functions! {
    env, "math-", {
        "inc" => (inc, 1..1, "Return NUMBER plus one."),
        "dec" => (dec, 1..1, "Return NUMBER minus one."),
    }
}
```

```emacs-lisp
(math-inc 5) ; 6
(math-inc 5 7) ; 'wrong-number-of-arguments
(math-dec "5") ; 'wrong-type-argument
```

## Returning anonymous lambdas
TODO
