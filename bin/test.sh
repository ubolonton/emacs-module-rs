#!/usr/bin/env bash

# TODO: Make this into an automated test.

here=`cd $(dirname $BASH_SOURCE); pwd`
root=`cd $here/..; pwd`
RS_MODULE=$(find $root -iname '*emacs_rs_module*.dylib' | head -n 1)
MODULE=$root/target/debug/libtest_module.dylib
FEATURE="'test-module"

read -r -d '' expr <<EOF
(progn
  (unless (featurep 'rs-module)
    (module-load "$RS_MODULE"))
  ;(rs-module/load "$MODULE")
  (module-load "$MODULE")
  (if (featurep $FEATURE)
      (message "Module was loaded successfully!" )
    (message "Module could not be loaded!"))

  (message "%s" (test-module/inc 5))

  (if (featurep $FEATURE)
    (test-module/test))
)
EOF

RUST_BACKTRACE=1 emacs -batch -eval "$expr"

echo '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
