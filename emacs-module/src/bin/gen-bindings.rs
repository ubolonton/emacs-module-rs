//! Generates the pre-built binding files kept in git.

include!(concat!(env!("CARGO_MANIFEST_DIR"), "/bindgen_config.rs"));

use std::path::PathBuf;

fn generate(header: &str, dest: &str) {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    bindgen_builder(base.join(header).to_str().unwrap())
        .generate()
        .expect("bindgen failed")
        .write_to_file(base.join(dest))
        .expect("write_to_file failed");
    println!("Generated {dest}");
}

fn main() {
    generate("include/emacs-module.h",    "src/emacs-module.rs");
    generate("include/emacs-module-28.h", "src/emacs-module-28.rs");
}
