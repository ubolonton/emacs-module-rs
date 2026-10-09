use emacs::{Env, Result};

emacs::plugin_is_GPL_compatible!();

// ANCHOR: example
// Assuming crate's name is `native_parallelism`.

#[emacs::module(separator = "/")]
fn init(_: &Env) -> Result<()> { Ok(()) }

mod shared_state {
    mod thread {
        use emacs::{defun, Result};

        // Ignore the nested mod's.
        // (native-parallelism/make-thread "name")
        #[defun(mod_in_name = false)]
        fn make_thread(name: String) -> Result<String> {
            Ok(name)
        }
    }

    mod process {
        use emacs::{defun, Result};

        // (native-parallelism/shared-state-process-launch "bckgrnd")
        #[defun]
        fn launch(name: String) -> Result<String> {
            Ok(name)
        }

        // Specify a name explicitly, since Rust identifier cannot contain `:`.
        // (native-parallelism/process:pool "http-client" 2 8)
        #[defun(mod_in_name = false, name = "process:pool")]
        fn pool(name: String, min: i64, max: i64) -> Result<String> {
            Ok(format!("{name}: {min}..{max}"))
        }
    }
}
// ANCHOR_END: example
