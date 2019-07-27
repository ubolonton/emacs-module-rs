$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$module_dir = "$project_root\target\debug"

Copy-Item $module_dir\emacs_rs_module.dll $module_dir\rs-module.dll
Copy-Item $module_dir\test_module.dll $module_dir\t.dll

emacs --version

$env:PROJECT_ROOT = $project_root
$env:MODULE_DIR = $module_dir

emacs --batch --directory "$module_dir" `
  -l ert `
  -l "$project_root\test-module\tests\main.el" `
  -f ert-run-tests-batch-and-exit
