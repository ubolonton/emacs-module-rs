# Rust Coding Rules
- ALWAYS attempt to add a test case for changed behavior
- ALWAYS read and copy the style of similar tests when adding new cases
- NEVER perform builds with the release profile, unless asked or reproducing performance issues
- AVOID using `panic!`, `unreachable!`, `.unwrap()`, unsafe code, and clippy rule ignores
- PREFER patterns like `if let` to handle fallibility
- PREFER `#[expect()]` over `[allow()]` if clippy must be disabled
- PREFER let chains (`if let` combined with `&&`) over nested `if let` statements
- PREFER top-level imports over local imports or fully qualified names
- AVOID shortening variable names, e.g., use `version` instead of `ver`
