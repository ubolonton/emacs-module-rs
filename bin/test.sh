#!/usr/bin/env bash

# FIX: Use Rust instead of bash to drive the tests.

here=`cd $(dirname $BASH_SOURCE); pwd`
source $here/env.sh

# rs-module
`cd $MODULE_DIR && ln -f -s libemacs_rs_module.$ext rs-module.so`

# test-module
`cd $MODULE_DIR && ln -f -s $MODULE_ORIGINAL $MODULE_RENAMED`

$EMACS --version
echo "Testing $MODULE_DIR/$MODULE_RENAMED"

$EMACS -batch -l ert \
       -l "$MODULE_DIR/$MODULE_RENAMED" \
       -l "$MODULE_DIR/rs-module.so" \
       -l "$PROJECT_ROOT/test-module/src/test.el" \
       -f ert-run-tests-batch-and-exit
