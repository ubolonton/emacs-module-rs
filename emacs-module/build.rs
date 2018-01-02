extern crate bindgen;

use std::env;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    bindgen::builder()
        .header("./include/emacs-module.h")
        .ctypes_prefix("::libc")
        .generate().unwrap()
        .write_to_file(Path::new(&out_dir).join("emacs_module.rs"))
        .unwrap();

}
