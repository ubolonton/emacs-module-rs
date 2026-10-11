use std::{
    env,
    path::{Component, Path},
};

use proc_macro::{Span, TokenStream};

/// Where a #[`defun`] is defined in its crate. Lisp tools use it to find the Rust source.
#[derive(Debug)]
pub struct Location {
    /// Path relative to the crate's root, with `/` separators.
    pub file: String,
    /// First line of the item, including the attributes above it, e.g. its docstring.
    pub line: u32,
}

impl Location {
    /// Returns the location of the item that the attribute macro being expanded is attached to.
    /// Returns `None` if the file is not under the crate's root, e.g. when it is generated.
    pub fn of_attributed_item(item: &TokenStream) -> Option<Self> {
        // `call_site` is the attribute itself. The item's first token is the first attribute that
        // remains on it, which may be above the macro's attribute (e.g. a docstring), or below it.
        let call_site = Span::call_site();
        let first_line = item
            .clone()
            .into_iter()
            .next()
            .map_or(call_site.line(), |token| token.span().line().min(call_site.line()));
        let line = u32::try_from(first_line).ok()?;
        let manifest_dir = env::var_os("CARGO_MANIFEST_DIR")?;
        let file = crate_relative(&call_site.local_file()?, Path::new(&manifest_dir))?;
        Some(Self { file, line })
    }
}

/// Returns `file` relative to `manifest_dir`, with `/` separators, so that Lisp code can join it
/// with a directory on any OS.
///
/// A relative `file` is resolved against the current directory. For a proc macro, that is rustc's,
/// which Cargo sets to the workspace root. So is the path `file!()` gives, e.g.
/// `crates/foo/src/lib.rs`.
fn crate_relative(file: &Path, manifest_dir: &Path) -> Option<String> {
    let file = file.canonicalize().ok()?;
    let manifest_dir = manifest_dir.canonicalize().ok()?;
    let mut parts = vec![];
    for component in file.strip_prefix(manifest_dir).ok()?.components() {
        match component {
            Component::Normal(part) => parts.push(part.to_str()?),
            _ => return None,
        }
    }
    Some(parts.join("/"))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn manifest_dir() -> &'static Path {
        Path::new(env!("CARGO_MANIFEST_DIR"))
    }

    #[test]
    fn absolute_file_in_crate() {
        let file = manifest_dir().join("src").join("source.rs");
        assert_eq!(crate_relative(&file, manifest_dir()).as_deref(), Some("src/source.rs"));
    }

    #[test]
    fn relative_file_resolved_against_current_dir() {
        // `cargo test` runs unit tests in the crate's root.
        let file = Path::new("src").join("source.rs");
        assert_eq!(crate_relative(&file, manifest_dir()).as_deref(), Some("src/source.rs"));
    }

    #[test]
    fn file_outside_crate() {
        let file = manifest_dir().join("..").join("emacs").join("src").join("lib.rs");
        assert_eq!(crate_relative(&file, manifest_dir()), None);
    }

    #[test]
    fn missing_file() {
        let file = manifest_dir().join("src").join("missing.rs");
        assert_eq!(crate_relative(&file, manifest_dir()), None);
    }
}
