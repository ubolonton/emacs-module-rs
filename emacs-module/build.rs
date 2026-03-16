#[cfg(feature = "bindgen-build")]
extern crate bindgen;

#[cfg(feature = "bindgen-build")]
include!("bindgen_config.rs");

fn main() {
    // Highest version first — features are additive in Cargo,
    // so emacs-29 = ["emacs-28"] means both are enabled.
    // To add version N:
    //   1. Add `cfg!(feature = "emacs-N") => "N",` at the top
    //   2. Add "N" to the check-cfg values
    //   3. Add `#[cfg(emacs_version = "N")]` include in lib.rs
    let version = if cfg!(feature = "emacs-28") { "28" } else { "25" };

    println!("cargo::rustc-check-cfg=cfg(emacs_version, values(\"25\", \"28\"))");
    println!("cargo::rustc-cfg=emacs_version=\"{version}\"");

    #[cfg(feature = "bindgen-build")]
    {
        use std::env;
        use std::path::Path;
        let out_dir = env::var("OUT_DIR").unwrap();
        let header = match version {
            "25" => "./include/emacs-module.h",
            v    => &format!("./include/emacs-module-{v}.h"),
        };
        bindgen_builder(header)
            .generate()
            .expect("bindgen failed")
            .write_to_file(Path::new(&out_dir).join("emacs-module.rs"))
            .expect("write_to_file failed");
    }
}
