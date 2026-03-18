$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$target = $args[0]

# Diagnostic: show which GCC/linker Rust will use for x86_64-pc-windows-gnu.
# The CRT (msvcrt.dll vs ucrtbase.dll) is determined by which GCC is in PATH.
Write-Host "=== Build environment ==="
$gcc = Get-Command gcc -ErrorAction SilentlyContinue
if ($gcc) {
    Write-Host "gcc: $($gcc.Source)"
    gcc --version 2>&1 | Select-Object -First 1
} else {
    Write-Host "gcc: not found in PATH"
}
Write-Host "========================="

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
Copy-Item $module_dir\test_module_28.dll $module_dir\t28.dll

# Diagnostic: show which CRT each module is linked against (msvcrt.dll vs ucrtbase.dll).
# This is the ground truth for CRT mismatch debugging.
Write-Host "=== CRT imports for built modules ==="
foreach ($dll in @("t.dll", "t28.dll", "rs-module.dll")) {
    $path = "$module_dir\$dll"
    if (Test-Path $path) {
        Write-Host "--- $dll ---"
        & objdump -p $path 2>$null | Select-String "DLL Name"
    }
}
Write-Host "====================================="
