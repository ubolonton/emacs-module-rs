use std::path::{Path, PathBuf};

use anyhow::Result;
use clap::{Parser, Subcommand};
use xshell::{Shell, cmd};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Build all crates and copy module artifacts into the target directory
    Build {
        #[arg(long)]
        release: bool,
    },
    /// Run integration tests (requires Emacs installed)
    Test {
        /// Re-run on file changes (requires cargo-watch)
        #[arg(long)]
        watch: bool,
        #[arg(long)]
        release: bool,
        /// Print shared-library diagnostics for Emacs and the built modules
        #[arg(long)]
        verbose: bool,
        /// Run only the ERT tests whose names match this regexp, e.g. `^error::`
        #[arg(long)]
        filter: Option<String>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::Build { release } => build(release),
        Command::Test { watch, release, verbose, filter } => {
            test(watch, release, verbose, filter.as_deref())
        }
    }
}

fn project_root() -> PathBuf {
    // CARGO_MANIFEST_DIR is xtask/; the workspace root is one level up.
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("xtask/ must have a parent directory")
        .to_owned()
}

fn ext() -> &'static str {
    if cfg!(target_os = "macos") {
        "dylib"
    } else if cfg!(windows) {
        "dll"
    } else {
        "so"
    }
}

// On Windows, Cargo omits the "lib" prefix for cdylib outputs (e.g. foo.dll not libfoo.dll).
fn lib_prefix() -> &'static str {
    if cfg!(windows) {
        ""
    } else {
        "lib"
    }
}

/// Resolves `name` to an absolute path by searching `PATH`.
///
/// `std::process::Command` does PATH resolution internally but does not expose the resolved path.
/// On Windows the PATH may also contain MSYS2-style entries (e.g. `/c/Users/...`) alongside native
/// ones (e.g. `C:\Users\...`); only native entries are usable by Windows tools like `objdump`.
fn resolve_in_path(name: &str) -> Option<PathBuf> {
    let p = Path::new(name);
    #[cfg(windows)]
    if p.has_root() {
        // A path starting with "/" looks absolute in MSYS2 but isn't on native Windows. Only accept
        // it as pre-resolved if it has a proper drive prefix (e.g. "C:\...").
        return matches!(p.components().next(), Some(std::path::Component::Prefix(_)))
            .then(|| p.to_owned());
    }
    #[cfg(not(windows))]
    if p.is_absolute() {
        return Some(p.to_owned());
    }
    let path_var = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path_var) {
        #[cfg(windows)]
        {
            // Skip MSYS2-style entries (e.g. "/c/...") — native Windows entries start with a drive
            // letter ("C:\...") or a UNC prefix ("\\...").
            let dir_s = dir.to_string_lossy();
            let is_native = dir_s.starts_with("\\\\")
                || (dir_s.chars().next().map_or(false, |c| c.is_ascii_alphabetic())
                    && dir_s.as_bytes().get(1) == Some(&b':'));
            if !is_native {
                continue;
            }
            // Prefer .exe so we get the PE binary, not a script/shim.
            for candidate in [dir.join(format!("{name}.exe")), dir.join(name)] {
                if candidate.is_file() {
                    return Some(candidate);
                }
            }
        }
        #[cfg(not(windows))]
        {
            let candidate = dir.join(name);
            if candidate.is_file() {
                return Some(candidate);
            }
        }
    }
    None
}

/// Prints the shared libraries that `path` links against. Best-effort.
fn print_shared_libs(sh: &Shell, path: &Path) {
    if cfg!(windows) {
        if let Ok(out) = cmd!(sh, "objdump -p {path}").output() {
            for line in String::from_utf8_lossy(&out.stdout).lines() {
                if line.contains("DLL Name") {
                    println!("{line}");
                }
            }
        }
    } else if cfg!(target_os = "macos") {
        if let Ok(out) = cmd!(sh, "otool -L {path}").output() {
            print!("{}", String::from_utf8_lossy(&out.stdout));
        }
    } else if let Ok(out) = cmd!(sh, "ldd {path}").output() {
        print!("{}", String::from_utf8_lossy(&out.stdout));
    }
}

fn build(release: bool) -> Result<()> {
    let root = project_root();
    let sh = Shell::new()?;
    sh.change_dir(&root);

    let profile = if release { "release" } else { "debug" };
    let release_flag: &[&str] = if release { &["--release"] } else { &[] };
    let target_dir = root.join("target");

    cmd!(sh, "cargo build --workspace --exclude xtask {release_flag...}").run()?;

    // test-module and test-module-28 are standalone workspaces (see their own Cargo.toml), each
    // with its own Cargo.lock, so they must be built in their own invocations. They still share
    // this workspace's target directory, so all module artifacts land in one place below.
    for member in ["test-module", "test-module-28"] {
        let manifest = root.join(member).join("Cargo.toml");
        cmd!(
            sh,
            "cargo build --manifest-path {manifest} --target-dir {target_dir} {release_flag...}"
        )
        .run()?;
    }

    let target = root.join("target").join(profile);
    let ext = ext();
    let prefix = lib_prefix();

    let copies = [
        (format!("{prefix}emacs_rs_module.{ext}"), format!("rs-module.{ext}")),
        (format!("{prefix}test_module.{ext}"), format!("t.{ext}")),
        (format!("{prefix}test_module_28.{ext}"), format!("t28.{ext}")),
    ];
    for (src, dst) in &copies {
        sh.copy_file(target.join(src), target.join(dst))?;
    }

    // Best-effort: show GCC version for build environment info.
    println!("=== Build environment ===");
    let _ = cmd!(sh, "gcc --version").run();
    println!("=========================");

    // Show which runtime libraries each module links against.
    // On Windows this reveals the CRT (msvcrt.dll vs ucrtbase.dll);
    // on Linux/macOS/FreeBSD it shows libc/libm/etc.
    println!("=== Shared libraries for built modules ===");
    for name in [format!("t.{ext}"), format!("t28.{ext}"), format!("rs-module.{ext}")] {
        println!("--- {name} ---");
        print_shared_libs(&sh, &target.join(&name));
    }
    println!("==========================================");

    Ok(())
}

/// Quote `s` as an Elisp string literal. Elisp needs escapes only for `\` and `"`.
fn elisp_string(s: &str) -> String {
    format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
}

/// Quote `s` as a single POSIX shell word, for commands that `cargo watch -s` runs through a shell.
fn shell_quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', "'\\''"))
}

fn test(watch: bool, release: bool, verbose: bool, filter: Option<&str>) -> Result<()> {
    let root = project_root();
    let sh = Shell::new()?;
    sh.change_dir(&root);

    if watch {
        // cargo-watch doesn't support passing flags through -s easily with spaces, so build and
        // test commands are kept as simple strings.
        let mut suffix = if release { " --release" } else { "" }.to_string();
        if verbose {
            suffix.push_str(" --verbose");
        }
        let build_cmd = format!("cargo xtask build{suffix}");
        if let Some(filter) = filter {
            suffix.push_str(&format!(" --filter {}", shell_quote(filter)));
        }
        let test_cmd = format!("cargo xtask test{suffix}");
        return cmd!(sh, "cargo watch -s {build_cmd} -s {test_cmd}").run().map_err(Into::into);
    }

    let profile = if release { "release" } else { "debug" };
    let target = root.join("target").join(profile);

    // Respect a custom Emacs binary if set in the environment.
    let emacs = std::env::var("EMACS").unwrap_or_else(|_| "emacs".to_string());

    cmd!(sh, "{emacs} --version").run()?;

    // Show which runtime libraries emacs links against. On Windows this reveals the CRT (msvcrt.dll
    // vs ucrtbase.dll); on Linux/macOS/FreeBSD it shows libc/libm/etc. Our modules must link the
    // same runtime or file-descriptor sharing will crash.
    if verbose {
        if let Some(emacs_path) = resolve_in_path(&emacs) {
            println!("=== Emacs binary: {} ===", emacs_path.display());
            print_shared_libs(&sh, &emacs_path);
            println!("===================================");
        }
    }

    // These env vars are read by the Lisp test helpers (e.g. t/run-in-sub-process uses PROJECT_ROOT
    // and MODULE_DIR to invoke emacs directly in a subprocess). Propagate EMACS so subprocesses
    // spawned by Lisp tests use the same binary.
    sh.set_var("PROJECT_ROOT", &root);
    sh.set_var("MODULE_DIR", &target);
    sh.set_var("EMACS_MODULE_RS_DEBUG", "1");
    sh.set_var("EMACS", &emacs);

    // ERT selector `t` selects all tests. A string selects tests whose names match it as a regexp.
    let selector = filter.map_or_else(|| "t".to_string(), elisp_string);
    let run_tests = format!("(ert-run-tests-batch-and-exit {selector})");

    println!("Testing test-module");
    let main_el = root.join("test-module/tests/main.el");
    cmd!(sh, "{emacs} -Q -batch --directory {target} -l ert -l {main_el} --eval {run_tests}").run()?;

    println!("Testing test-module-28");
    let main_el_28 = root.join("test-module-28/tests/main.el");
    cmd!(sh, "{emacs} -Q -batch --directory {target} -l ert -l {main_el_28} --eval {run_tests}").run()?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn elisp_string_escapes_backslash_and_quote() {
        assert_eq!(elisp_string(r#"^error::\|"x""#), r#""^error::\\|\"x\"""#);
    }

    #[test]
    fn shell_quote_escapes_single_quote() {
        assert_eq!(shell_quote(r"^a\|b's"), r"'^a\|b'\''s'");
    }
}
