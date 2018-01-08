#!/usr/bin/env bash

# (Re)load test-module into a running Emacs instance.

here=`cd $(dirname $BASH_SOURCE); pwd`
root=`cd $here/..; pwd`
RS_MODULE=$(find $root -iname '*emacs_rs_module*.dylib' | head -n 1)
MODULE=$root/target/debug/libtest_module.dylib

read -r -d '' expr <<EOF
(progn
  (unless (featurep 'rs-module)
    (module-load "$RS_MODULE"))
  (rs-module/load "$MODULE"))
EOF

emacsclient -e "$expr"

echo '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
