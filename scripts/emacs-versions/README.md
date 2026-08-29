# Emacs Versions Tool

This tool builds GNU Emacs from source. It installs each build as a
separate Debian package. You can install many Emacs versions at the
same time.

Use this tool to test `emacs-module-rs` against different Emacs
versions.

## Requirements

You need a Debian system. You need a local git checkout of the Emacs
source code. The default location is `~/Programming/Tools/emacs`. You
can clone it from `https://github.com/emacs-mirror/emacs`.

## How Naming Works

The tool reads the Emacs version from the source checkout. The tool
also reads the current git commit hash.

The tool builds a name from the version and the hash. For example:
`emacs-30.2-g1a2b3c4d5e6f`.

Each name is unique. You can install two builds from the same Emacs
version at different commits. The names will not clash.

The tool installs each build under `/opt/<name>`. The tool also adds
a command to `/usr/bin/<name>`. Run this command to start that Emacs
build.

## Before You Build

1. Go to the Emacs source checkout.
2. Check out the git tag or branch you want to build. For example:
   `git checkout emacs-30.2`.
3. Run the build command from the tool.

The tool always builds the commit that is currently checked out. It
does not accept a version number as an argument.

## Commands

Run all commands from `scripts/emacs-versions/emacs-versions.sh`.

### Check Build Dependencies

```
emacs-versions.sh deps status
```

This command lists the packages the build needs. It shows which
packages are installed and which are missing.

### Install Build Dependencies

```
emacs-versions.sh deps install
```

This command installs the missing packages. It uses `sudo apt-get`.

### Remove Build Dependencies

```
emacs-versions.sh deps remove
```

This command removes the build packages. It asks for confirmation
first. Some of these packages may be in use by other tools on your
system. Check the list before you confirm.

### Build a Package

```
emacs-versions.sh build
```

This command builds Emacs from the current source checkout. It
creates a `.deb` package. It does not install the package.

The command prints the path to the `.deb` file at the end.

### Build and Install

```
emacs-versions.sh install
```

This command builds Emacs and installs the package. It uses
`sudo dpkg -i`.

### List Installed Versions

```
emacs-versions.sh list
```

This command lists the Emacs versions this tool installed. It shows
the package name, the version, and the install location.

### Remove an Installed Version

```
emacs-versions.sh uninstall <package-name>
```

This command removes one installed Emacs version. Use the package
name from the `list` command.

This command does not affect other installed Emacs versions.

## Options

You can add these options to `build` and `install`:

| Option | Effect |
|---|---|
| `--source-dir DIR` | Use a different Emacs source checkout. |
| `--prefix-root DIR` | Install under a different root directory. The default is `/opt`. |
| `--build-root DIR` | Use a different directory for build files. |
| `--jobs N` | Set the number of parallel compile jobs. |

## Build Configuration

The tool builds Emacs with these features:

- Dynamic modules
- Native compilation
- No graphical interface (no X)
- GnuTLS, D-Bus, SQLite3, tree-sitter, systemd, SELinux, ACL, GPM,
  sound (ALSA), file notification (inotify)

This configuration is close to the configuration of the Debian
`emacs-nox` package.

Some older Emacs versions do not support all these features. The
build process ignores flags a given version does not support. Check
the build log to see which features were actually enabled.

## Limitations

This tool builds one Emacs version per run. It builds the commit
that is currently checked out in the source directory. It does not
build multiple versions in a single run.

This tool only supports Debian and Debian-based systems.
