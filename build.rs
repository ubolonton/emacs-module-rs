use std::process::exit;

extern crate rustc_version;
use rustc_version::{version, Version};

fn main() {
    let version = version().unwrap();
    if version < Version::parse("1.56.0").unwrap() {
        eprintln!("emacs-module-rs requires rustc 1.56.0 or newer, got {}", version);
        exit(1);
    }
}
