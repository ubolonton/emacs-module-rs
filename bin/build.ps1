$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName

$target = $args[0]
if ($target -eq "release") {
    $extra = "--release"
} else {
    $target = "debug"
    $extra = ""
}

$module_dir = "$project_root\target\$target"

Push-Location $project_root

cargo build --all $extra

Pop-Location

Copy-Item $module_dir\emacs_rs_module.dll $module_dir\rs-module.dll
Copy-Item $module_dir\test_module.dll $module_dir\t.dll
