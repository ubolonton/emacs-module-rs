#!/usr/bin/env bash

# TODO: Use Rust instead of bash to drive the tests.

here=`cd $(dirname $BASH_SOURCE); pwd`
root=`cd $here/..; pwd`

MODULE_DIR=$root/target/debug

# rs-module
`cd $MODULE_DIR && ln -f -s libemacs_rs_module.dylib rs-module.so`

# test-module
MODULE_ORIG_FILE=libtest_module.dylib
MODULE_FILE=test-module.so
`cd $MODULE_DIR && ln -f -s $MODULE_ORIG_FILE $MODULE_FILE`
MODULE=$MODULE_DIR/$MODULE_FILE

EMACS=emacs

RUST_BACKTRACE=1 $EMACS -batch -l ert -l $MODULE -l $MODULE_DIR/rs-module.so -l $root/test-module/src/test.el -f ert-run-tests-batch-and-exit
