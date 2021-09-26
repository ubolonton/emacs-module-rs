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
            .generate()
            .unwrap()
            .write_to_file(Path::new(&out_dir).join("emacs-module.rs"))
            .unwrap();

        // bindgen \
        //         --no-prepend-enum-name \
        //         --allowlist-type '^emacs.*' \
        //         --allowlist-function '^emacs.*' \
        //         --allowlist-var '^emacs.*' \
        //         emacs-module/include/emacs-module.h \
        //         -o emacs-module/src/emacs_module.rs
    }
}
