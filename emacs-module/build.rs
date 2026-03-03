#[cfg(feature = "bindgen")]
extern crate bindgen;

fn main() {
    #[cfg(feature = "bindgen")]
    {
        use std::env;
        use std::path::Path;
        let out_dir = env::var("OUT_DIR").unwrap();
        bindgen::builder()
            .header("./include/emacs-module.h")
            .allowlist_type("^emacs.*")
            .allowlist_function("^emacs.*")
            .allowlist_var("^emacs.*")
            .prepend_enum_name(false)
            // XXX: I don't know why bindgen maps this to c_long, which doesn't work on Windows. On
            //  the flip side, I agree with the opinion that `intmax_t` is bad in the first place.
            //
            // TODO: Restrict support to 64-bit platforms.
            .blocklist_type("intmax_t").raw_line("pub type intmax_t = i64;")
            .generate()
            .unwrap()
            .write_to_file(Path::new(&out_dir).join("emacs-module.rs"))
            .unwrap();
    }

    let _bindgen_command = "
bindgen \
    --blocklist-type intmax_t --raw-line 'pub type intmax_t = i64;' \
    --no-prepend-enum-name \
    --allowlist-type '^emacs.*' \
    --allowlist-function '^emacs.*' \
    --allowlist-var '^emacs.*' \
    emacs-module/include/emacs-module.h \
    -o emacs-module/src/emacs-module.rs
";
}
