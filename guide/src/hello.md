# Hello, Emacs!

Create a new project:

```bash
cargo new greeting
cd greeting
```

Modify `Cargo.toml`:

```toml
[lib]
crate-type = ["cdylib"]

[dependencies]
emacs = "0.5.0"
```

Write code in `src/lib.rs`:

```rust
#[macro_use]
extern crate emacs;

use emacs::{Env, Result, Value};

// Emacs won't load the module without this.
emacs_plugin_is_GPL_compatible!();

// Declare and define the init function, which Emacs will call when it loads the module.
emacs_module_init!(init);
fn init(env: &Env) -> Result<Value> {
    env.message("Hello, Emacs!")?;
    env.provide("greeting")
}
```

Build the module and create a symlink with `.so` extension so that Emacs can recognize it:

```bash
cargo build
cd target/debug
# If you are on Linux, it would be libgreeting.so
ln -s libgreeting.dylib greeting.so
```

Add `target/debug` to your Emacs's `load-path`, then load the module:

```emacs-lisp
(require 'greeting)
```

The minibuffer should display the message "Hello, Emacs!".
