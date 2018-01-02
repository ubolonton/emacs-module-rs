Emacs Module Bindings
====

This crate provides access to the new `Emacs module` functionality recently
introduced in Emacs 25.

Usage aka How to write an oxidized Emacs module in a few easy steps
----
1. Create a new Cargo `lib` project, say `my_fancy_module`
2. Open up `Cargo.toml` in an editor, and:
   * Add `crate-type = ["cdylib"]` to the `[lib]` section
   * Add the following dependencies:
   ```` toml
   libc = "0.2.34"
   emacs = { path = "$EMB_PATH" }
   ````
3. Add the following to your `src/lib.rs`:
   ```` Rust
   extern crate libc;
   extern crate emacs;

   use emacs::{Env, EmacsRT};

   /// This states that the module is GPL-compliant.
   /// Emacs won't load the module if this symbol is undefined.
   #[no_mangle]
   #[allow(non_upper_case_globals)]
   pub static plugin_is_GPL_compatible: libc::c_int = 0;

   #[no_mangle]
   pub extern "C" fn emacs_module_init(ert: *mut EmacsRT) -> libc::c_int {
       let env = Env::from(ert);

       // Add any other things you need the module to do here

       env.provide("my-fancy-module");
       0
   }
   ````
4. Execute `cargo build`
5. If you're on OS X, copy `target/debug/libmy_fancy_module.dylib`
    to `target/debug/libmy_fancy_module.so`
6. Load it in emacs with `(require 'my-fancy-module "/path/to/libmy_fancy_module.so")`.
   Note that this requires Emacs to be configured and compiled with
   the `--with-modules` flag.

For a more elaborate example, check out [test-module](test-module).

Development
----
- Building:
    ````shell
    cargo build --all
    ````
- Testing:
    ````shell
    bin/test.sh
    ````
- Continuous testing (requires `cargo-watch`):
    ````shell
    cargo watch -x 'build --all' -s bin/test.sh
    ````
