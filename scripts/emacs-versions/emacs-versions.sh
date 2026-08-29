#!/usr/bin/env bash
# Build GNU Emacs from a local source checkout and install it as a dpkg
# package, so multiple versions (e.g. 29 / 30 / 31 / master) can coexist on
# the same Debian machine for testing emacs-module-rs against each of them.
#
# Usage:
#   emacs-versions.sh deps status|install|remove
#   emacs-versions.sh build   [--source-dir DIR] [--prefix-root DIR] [--jobs N]
#   emacs-versions.sh install [--source-dir DIR] [--prefix-root DIR] [--jobs N]
#   emacs-versions.sh list
#   emacs-versions.sh uninstall <package-name>
#
# `build`/`install` always operate on whatever commit is currently checked
# out in the source dir; check out the tag/branch you want before running.
set -euo pipefail

DEFAULT_SOURCE_DIR="$HOME/Programming/Tools/emacs"
SOURCE_DIR="${EMACS_VERSIONS_SOURCE_DIR:-$DEFAULT_SOURCE_DIR}"
BUILD_ROOT="${EMACS_VERSIONS_BUILD_ROOT:-}"
PREFIX_ROOT="${EMACS_VERSIONS_PREFIX_ROOT:-/opt}"
JOBS="${EMACS_VERSIONS_JOBS:-$(nproc)}"

# Packages needed to build a config close to Debian's own emacs-nox
# (dynamic modules, native comp, no GUI/X) across Emacs 29-32, plus `file`
# for classifying ELF binaries when computing package dependencies below.
BUILD_PACKAGES=(
  build-essential
  autoconf
  texinfo
  pkg-config
  gperf
  file
  libgnutls28-dev
  libncurses-dev
  libjansson-dev
  libgccjit-14-dev
  gcc-14
  libgpm-dev
  liblcms2-dev
  libtree-sitter-dev
  libselinux1-dev
  libacl1-dev
  libdbus-1-dev
  libsystemd-dev
  libxml2-dev
  libsqlite3-dev
  zlib1g-dev
  libasound2-dev
)

log() { printf '\033[1;34m==>\033[0m %s\n' "$*" >&2; }
err() { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; }
die() { err "$*"; exit 1; }

usage() {
  sed -n '2,14p' "$0" | sed 's/^# \{0,1\}//'
}

require_source_dir() {
  [ -d "$SOURCE_DIR" ] || die "source dir not found: $SOURCE_DIR"
  [ -f "$SOURCE_DIR/configure.ac" ] || die "$SOURCE_DIR does not look like an Emacs source checkout (no configure.ac)"
  git -C "$SOURCE_DIR" rev-parse --git-dir >/dev/null 2>&1 || die "$SOURCE_DIR is not a git checkout (needed to derive a build name)"
}

# --- deps --------------------------------------------------------------

missing_packages() {
  local pkg missing=()
  for pkg in "${BUILD_PACKAGES[@]}"; do
    dpkg -s "$pkg" >/dev/null 2>&1 || missing+=("$pkg")
  done
  printf '%s\n' "${missing[@]}"
}

deps_status() {
  local pkg
  for pkg in "${BUILD_PACKAGES[@]}"; do
    if dpkg -s "$pkg" >/dev/null 2>&1; then
      printf '  [installed] %s\n' "$pkg"
    else
      printf '  [missing]   %s\n' "$pkg"
    fi
  done
}

deps_install() {
  local missing=()
  mapfile -t missing < <(missing_packages)
  if [ "${#missing[@]}" -eq 0 ]; then
    log "all build dependencies already installed"
    return 0
  fi
  log "installing missing build dependencies: ${missing[*]}"
  sudo apt-get install -y "${missing[@]}"
}

deps_remove() {
  log "this will run: sudo apt-get remove ${BUILD_PACKAGES[*]}"
  log "some of these (build-essential, autoconf, texinfo, pkg-config, ...) may be relied on by other tools on this machine."
  read -r -p "Continue? [y/N] " reply
  case "$reply" in
    y|Y|yes|YES) ;;
    *) log "aborted"; return 1 ;;
  esac
  sudo apt-get remove -y "${BUILD_PACKAGES[@]}"
}

# --- naming --------------------------------------------------------------

emacs_version() {
  sed -n 's/^AC_INIT(\[GNU Emacs\], \[\([0-9.]*\)\].*/\1/p' "$SOURCE_DIR/configure.ac"
}

build_name() {
  local version hash dirty=""
  version="$(emacs_version)"
  [ -n "$version" ] || die "could not determine Emacs version from $SOURCE_DIR/configure.ac"
  hash="$(git -C "$SOURCE_DIR" rev-parse --short=12 HEAD)"
  # Only tracked-file changes count as dirty; unrelated untracked files
  # (e.g. editor state left over from browsing the checkout) should not
  # force a new build name.
  { git -C "$SOURCE_DIR" diff --quiet && git -C "$SOURCE_DIR" diff --cached --quiet; } || dirty="-dirty"
  printf 'emacs-%s-g%s%s\n' "$version" "$hash" "$dirty"
}

# --- build -----------------------------------------------------------------

CONFIGURE_ARGS=()
DEB_PATH=""
build_configure_args() {
  local prefix="$1"
  CONFIGURE_ARGS=(
    "--prefix=$prefix"
    --with-x=no
    --without-gconf
    --without-gsettings
    --without-pop
    --without-mailutils
    --with-modules
    --with-native-compilation=yes
    --with-json
    --with-gnutls
    --with-libsystemd
    --with-sqlite3
    --with-tree-sitter
    --with-xml2
    --with-zlib
    --with-threads
    --with-gpm
    --with-lcms2
    --with-dbus
    --with-selinux
    --with-pdumper=yes
    --with-sound=alsa
    --with-file-notification=inotify
  )
}

elf_files_under() {
  # -N disables filename padding; without it, `file` column-aligns mime
  # types when scanning many files at once, breaking the `: ` field split.
  find "$1" -type f -print0 | xargs -0 -r file -N --mime-type | awk -F': ' '$2 == "application/x-executable" || $2 == "application/x-sharedlib" || $2 == "application/x-pie-executable" {print $1}'
}

do_build() {
  require_source_dir
  deps_install

  local name version prefix work_dir obj_dir pkg_dir
  name="$(build_name)"
  version="$(emacs_version)"
  prefix="$PREFIX_ROOT/$name"
  work_dir="${BUILD_ROOT:-$(dirname "$SOURCE_DIR")/emacs-versions-build}/$name"
  obj_dir="$work_dir/obj"
  pkg_dir="$work_dir/debian/$name"

  log "building $name (prefix: $prefix)"
  rm -rf "$work_dir"
  mkdir -p "$obj_dir" "$pkg_dir"

  log "cleaning generated build artifacts from a previous checkout"
  # Emacs compiles Lisp in-tree (lisp/*.elc, lisp/*loaddefs.el) even with an
  # out-of-tree configure/make; leftovers from building a different commit
  # (e.g. after switching branches) can look "up to date" to make and get
  # reused, producing broken/inconsistent bootstraps. autom4te.cache can
  # similarly make autoreconf silently reuse outdated macro expansions.
  # Scope the clean to Emacs's own source subdirectories, never the repo
  # root, so unrelated dotfiles the user keeps there are left alone.
  git -C "$SOURCE_DIR" clean -fdX -- \
    lisp leim src lib-src lib nt java admin build-aux doc etc info exec msdos m4 \
    aclocal.m4 configure config.log
  (cd "$SOURCE_DIR" && ./autogen.sh all)

  log "configuring"
  build_configure_args "$prefix"
  (cd "$obj_dir" && "$SOURCE_DIR/configure" "${CONFIGURE_ARGS[@]}")

  log "compiling with $JOBS jobs"
  make -C "$obj_dir" -j"$JOBS"

  log "installing into staging root"
  # DESTDIR + --prefix=$prefix (absolute) lands files at $pkg_dir$prefix,
  # i.e. $pkg_dir mirrors the target filesystem root.
  make -C "$obj_dir" install DESTDIR="$pkg_dir"

  mkdir -p "$pkg_dir/usr/bin"
  ln -sf "$prefix/bin/emacs" "$pkg_dir/usr/bin/$name"

  local stub_control="$work_dir/debian/control"
  {
    echo "Source: emacs-versions-build"
    echo "Section: editors"
    echo "Priority: optional"
    echo "Maintainer: $(git -C "$SOURCE_DIR" config user.name 2>/dev/null || echo "$(id -un)") <$(git -C "$SOURCE_DIR" config user.email 2>/dev/null || echo "$(id -un)@localhost")>"
    echo "Build-Depends: debhelper-compat (= 13)"
    echo
    echo "Package: $name"
    echo "Architecture: any"
    echo 'Depends: ${shlibs:Depends}, ${misc:Depends}'
    echo "Description: GNU Emacs $version (source build)"
  } > "$stub_control"

  log "computing runtime dependencies (dpkg-shlibdeps)"
  local elf=() depends=""
  mapfile -t elf < <(elf_files_under "$pkg_dir")
  if [ "${#elf[@]}" -gt 0 ]; then
    depends="$(cd "$work_dir" && dpkg-shlibdeps -O --ignore-missing-info "${elf[@]}" 2>/dev/null | sed -n 's/^shlibs:Depends=//p')"
  fi
  [ -n "$depends" ] || log "dpkg-shlibdeps found no dynamic dependencies (unexpected but not fatal)"

  local arch installed_size
  arch="$(dpkg --print-architecture)"
  installed_size="$(du -sk "$pkg_dir" | cut -f1)"

  mkdir -p "$pkg_dir/DEBIAN"
  {
    echo "Package: $name"
    echo "Version: ${version}+git$(git -C "$SOURCE_DIR" rev-parse --short=12 HEAD)"
    echo "Section: editors"
    echo "Priority: optional"
    echo "Architecture: $arch"
    echo "Installed-Size: $installed_size"
    [ -n "$depends" ] && echo "Depends: $depends"
    echo "Maintainer: $(git -C "$SOURCE_DIR" config user.name 2>/dev/null || echo "$(id -un)") <$(git -C "$SOURCE_DIR" config user.email 2>/dev/null || echo "$(id -un)@localhost")>"
    echo "Description: GNU Emacs $version (source build)"
    echo " Built from $SOURCE_DIR at commit $(git -C "$SOURCE_DIR" rev-parse --short=12 HEAD) for local multi-version testing."
    echo " Installed under $prefix; run via /usr/bin/$name."
  } > "$pkg_dir/DEBIAN/control"

  DEB_PATH="$work_dir/${name}.deb"
  dpkg-deb --build --root-owner-group "$pkg_dir" "$DEB_PATH"
  log "built $DEB_PATH"
}

do_install() {
  do_build
  log "installing $DEB_PATH"
  sudo dpkg -i "$DEB_PATH"
}

do_list() {
  local pkg ver status found=0
  while IFS=$'\t' read -r pkg ver status; do
    [[ "$status" == *"install ok installed"* ]] || continue
    printf '%s\t%s\t%s\n' "$pkg" "$ver" "$PREFIX_ROOT/$pkg"
    found=1
  done < <(dpkg-query -W -f='${Package}\t${Version}\t${Status}\n' 'emacs-*-g[0-9a-f]*' 2>/dev/null || true)
  [ "$found" -eq 1 ] || log "no emacs-versions packages installed"
}

do_uninstall() {
  local name="${1:?usage: emacs-versions.sh uninstall <package-name>}"
  dpkg -s "$name" >/dev/null 2>&1 || die "package not installed: $name (see: emacs-versions.sh list)"
  sudo dpkg -P "$name"
}

# --- arg parsing -----------------------------------------------------------

parse_common_opts() {
  while [ $# -gt 0 ]; do
    case "$1" in
      --source-dir) SOURCE_DIR="$2"; shift 2 ;;
      --prefix-root) PREFIX_ROOT="$2"; shift 2 ;;
      --build-root) BUILD_ROOT="$2"; shift 2 ;;
      --jobs) JOBS="$2"; shift 2 ;;
      *) EXTRA_ARGS+=("$1"); shift ;;
    esac
  done
}

main() {
  local cmd="${1:-}"
  [ $# -gt 0 ] && shift
  EXTRA_ARGS=()
  parse_common_opts "$@"
  set -- "${EXTRA_ARGS[@]}"

  case "$cmd" in
    deps)
      case "${1:-status}" in
        status) deps_status ;;
        install) deps_install ;;
        remove) deps_remove ;;
        *) die "unknown deps subcommand: $1" ;;
      esac
      ;;
    build) do_build; printf '%s\n' "$DEB_PATH" ;;
    install) do_install ;;
    list) do_list ;;
    uninstall) do_uninstall "${1:-}" ;;
    -h|--help|help|"") usage ;;
    *) die "unknown command: $cmd (see --help)" ;;
  esac
}

main "$@"
