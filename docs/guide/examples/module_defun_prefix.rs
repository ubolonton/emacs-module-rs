use emacs::{Env, Result};

emacs::plugin_is_GPL_compatible!();

// ANCHOR: example
    // The whole package contains other Lisp files, so the module is named
    // `tree-sitter-dyn`. But we want functions to be `tree-sitter-something`,
    // not `tree-sitter-dyn-something`.
    #[emacs::module(name = "tree-sitter-dyn", defun_prefix = "tree-sitter")]
    fn init(_: &Env) -> Result<()> { Ok(()) }
// ANCHOR_END: example
