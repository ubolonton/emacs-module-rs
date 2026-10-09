# Hello, Emacs!

Create a new project:

```bash
cargo new --lib greeting
cd greeting
cargo add emacs
```

Add to `Cargo.toml`:

```toml
[lib]
crate-type = ["cdylib"]
```

Write code in `src/lib.rs`:

```rust
{{#include ../examples/hello.rs}}
```

Build the module and create a symlink with `.so` extension so that Emacs can recognize it:

```bash
cargo build
cd target/debug

# If you are on Linux
ln -s libgreeting.so greeting.so

# If you are on macOS
ln -s libgreeting.dylib greeting.so
```

Add `target/debug` to your Emacs's `load-path`, then load the module:
```elisp
(add-to-list 'load-path "/path/to/target/debug")
(require 'greeting)
(greeting-say-hello "Emacs")
```

The minibuffer should display the message `Hello, Emacs!`.
