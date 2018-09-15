#!/usr/bin/env bash

# (Re)load test-module into a running Emacs instance.

here=`cd $(dirname $BASH_SOURCE); pwd`
source $here/env.sh

RS_MODULE=$(find $PROJECT_ROOT -iname "*emacs_rs_module*.$ext" | head -n 1)
MODULE="$MODULE_DIR/$MODULE_ORIGINAL"

read -r -d '' expr <<EOF
(progn
  (unless (featurep 'rs-module)
    (module-load "$RS_MODULE"))
  (rs-module/load "$MODULE"))
EOF

echo $expr

emacsclient -e "$expr"

echo '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
