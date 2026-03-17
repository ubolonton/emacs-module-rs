set -euo pipefail

system=$(uname)
if [[ $system == "Linux" || $system == "FreeBSD" ]]; then
    EXT="so"
elif [[ $system == "Darwin" ]]; then
    EXT="dylib"
else
    EXT="dll"
fi

here=$(cd "$(dirname "$BASH_SOURCE")"; pwd)

PROJECT_ROOT=$(cd "$here/.."; pwd)
export PROJECT_ROOT

export EXT
# On Windows, Cargo omits the "lib" prefix for cdylib outputs (e.g. foo.dll not libfoo.dll).
if [[ $EXT == "dll" ]]; then LIB_PREFIX=""; else LIB_PREFIX="lib"; fi
export LIB_PREFIX
export TARGET=${TARGET:-debug}
export MODULE_DIR="$PROJECT_ROOT/target/$TARGET"
export MODULE_ORIGINAL=${MODULE_ORIGINAL:-${LIB_PREFIX}test_module.$EXT}
export MODULE_NAME=${MODULE_NAME:-t}
export MODULE_RENAMED=${MODULE_NAME}.$EXT
export MODULE_FULL="$MODULE_DIR/$MODULE_RENAMED"
export EMACS=${EMACS:-emacs}
