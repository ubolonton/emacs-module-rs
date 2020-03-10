$fn = $args[0]

emacs --batch --directory "$env:MODULE_DIR" `
  --module-assertions `
  -l "$env:PROJECT_ROOT\test-module\tests\main.el" -f "$fn"
