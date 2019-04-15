set -euo pipefail

system=`uname`
if [[ $system == "Linux" ]]; then
    ext="so"
elif [[ $system == "Darwin" ]]; then
    ext="dylib"
else
    echo "Unsupported system: $system"
    exit 1
fi

here=`cd $(dirname $BASH_SOURCE); pwd`

export PROJECT_ROOT=`cd $here/..; pwd`
export MODULE_DIR="$PROJECT_ROOT/target/debug"
export MODULE_ORIGINAL=${MODULE_ORIGINAL:-libtest_module.$ext}
export MODULE_NAME=${MODULE_NAME:-t}
export MODULE_RENAMED=${MODULE_NAME}.so
export MODULE_FULL="$MODULE_DIR/$MODULE_RENAMED"
export EMACS=${EMACS:-emacs}
export RUST_BACKTRACE=${RUST_BACKTRACE:-0}
