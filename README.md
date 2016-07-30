Emacs Module Bindings
====

This crate provides access to the new `Emacs module` functionality recently
introduced in Emacs 25. It's a basic FFI with a relatively straightforward
API. Have have a look at the source for details.

Usage aka How to write an oxidized Emacs module in a few easy steps
----
0. Clone this project to some `$EMB_PATH`
1. Create a new Cargo `lib` project, say `my_fancy_module`
2. Open up `Cargo.toml` in an editor, and:
   * Add `crate-type = ["cdylib"]` to the `[lib]` section (NOTE: Only
     Rust nightly correctly handles this at the moment)
   * Add the following dependencies:
   ```` toml
   libc = "0.2.14"
   emacs_module_bindings = { path = "$EMB_PATH" }
   ````
3. Add the following to your `src/lib.rs`:
   ```` Rust
   extern crate libc;
   extern crate zmq_ffi as zmq;
   extern crate emacs_module_bindings as emacs;

   use emacs::emacs_module::{EmacsEnv, EmacsRT, EmacsVal};

   /// This states that the module is GPL-compliant.
   /// Emacs won't load the module if this symbol is undefined.
   #[no_mangle]
   #[allow(non_upper_case_globals)]
   pub static plugin_is_GPL_compatible: libc::c_int = 0;

   #[no_mangle]
   pub extern "C" fn emacs_module_init(ert: *mut EmacsRT) -> libc::c_int {
       let env = emacs::get_environment(ert);

       // Add any other things you need the module to do here

       emacs::provide(env, "my-fancy-module");
       0
   }
   ````
4. Execute `cargo build`
5. If you're on OS X, copy `target/debug/libmy_fancy_module.dylib`
    to `target/debug/libmy_fancy_module.so`
6. Load it in emacs with `(require 'my-fancy-module "/path/to/libmy_fancy_module.so")`.
   Note that this requires Emacs to be configured and compiled with
   the `--with-modules` flag.
