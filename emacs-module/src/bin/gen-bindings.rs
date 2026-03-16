//! Generates the pre-built binding files kept in git.
//!
//! Run with:
//!   cargo run -p emacs_module --features bindgen --bin gen-bindings
//!
//! Writes to a temp directory first, then copies to src/, to avoid virtiofs
//! corruption on Windows VM (rustfmt truncate-and-rewrite fails on virtiofs).

include!(concat!(env!("CARGO_MANIFEST_DIR"), "/bindgen_config.rs"));

use std::path::PathBuf;

fn generate(header: &str, dest: &str) {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let header_path = base.join(header);
    let dest_path = base.join(dest);

    // Safe: dest is always a literal path with a filename component.
    let tmp_path = std::env::temp_dir().join(dest_path.file_name().unwrap());

    bindgen_builder(header_path.to_str().unwrap())
        .generate()
        .expect("bindgen failed")
        .write_to_file(&tmp_path)
        .expect("write_to_file failed");

    std::fs::copy(&tmp_path, &dest_path).expect("copy failed");
    let _ = std::fs::remove_file(&tmp_path);
    println!("Generated {dest}");
}

fn main() {
    generate("include/emacs-module.h",    "src/emacs-module.rs");
    generate("include/emacs-module-28.h", "src/emacs-module-28.rs");
}
