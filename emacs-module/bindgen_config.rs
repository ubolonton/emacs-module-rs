// Shared bindgen configuration for both build.rs (compile-time generation)
// and src/bin/gen-bindings.rs (pre-generating committed binding files).
//
// Included via include!() by both callers — not part of the library's module tree.
//
// All flags must match across both callers. When adding a new Emacs version:
//   1. Add a new header file under emacs-module/include/
//   2. Add an entry in src/bin/gen-bindings.rs
//   3. Add a #[cfg(emacs_version = "N")] include in src/lib.rs

fn bindgen_builder(header: &str) -> bindgen::Builder {
    bindgen::builder()
        .header(header)
        .allowlist_type("^emacs.*")
        .allowlist_function("^emacs.*")
        .allowlist_var("^emacs.*")
        .prepend_enum_name(false)

        // In a Windows VM, rustfmt failed with this error.
        //
        // The volume does not contain a recognized file system.
        // Please make sure that all required file system drivers are loaded and that the volume is not corrupted. (os error 1005)
        // Failed to run rustfmt: Internal rustfmt error (non-fatal, continuing)
        .formatter(bindgen::Formatter::Prettyplease)

        // intmax_t: bindgen maps this to c_long, which is i32 on Windows. Force i64.
        // TODO: Restrict support to 64-bit platforms.
        .blocklist_type("intmax_t").raw_line("pub type intmax_t = i64;")
        //   __intmax_t → drop (Linux-only, unused in emacs_* types)
        .blocklist_type("__intmax_t")

        // Cross-platform timespec (emacs-28): Linux emits __time_t/__syscall_slong_t as
        // c_long aliases, which become i32 on Windows, breaking timespec layout. Replace:
        //   __time_t → i64 (time_t is 64-bit on all supported 64-bit platforms)
        //   __syscall_slong_t → c_long (i64 Linux, i32 Windows; timespec stays 16 bytes)
        .blocklist_type("__time_t").raw_line("pub type __time_t = i64;")
        .blocklist_type("__syscall_slong_t")
            .raw_line("pub type __syscall_slong_t = ::std::os::raw::c_long;")
}
