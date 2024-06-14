set -euo pipefail

system=$(uname)
if [[ $system == "Linux" || $system == "FreeBSD" ]]; then
    EXT="so"
elif [[ $system == "Darwin" ]]; then
    EXT="dylib"
else
    echo "Unsupported system: $system"
    exit 1
fi

here=$(cd "$(dirname "$BASH_SOURCE")"; pwd)

PROJECT_ROOT=$(cd "$here/.."; pwd)
export PROJECT_ROOT

export EXT
export TARGET=${TARGET:-debug}
export MODULE_DIR="$PROJECT_ROOT/target/$TARGET"
export MODULE_ORIGINAL=${MODULE_ORIGINAL:-libtest_module.$EXT}
export MODULE_NAME=${MODULE_NAME:-t}
export MODULE_RENAMED=${MODULE_NAME}.so
export MODULE_FULL="$MODULE_DIR/$MODULE_RENAMED"
export EMACS=${EMACS:-emacs}
