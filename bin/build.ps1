$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$target = $args[0]

Push-Location $project_root
try {
    switch ($target) {
        'release' {
            $module_dir = "$project_root\target\release"
            cargo build --all --release
        }
        default {
            $module_dir = "$project_root\target\debug"
            cargo build --all
        }
    }
} finally {
    Pop-Location
}

Copy-Item $module_dir\emacs_rs_module.dll $module_dir\rs-module.dll
Copy-Item $module_dir\test_module.dll $module_dir\t.dll
