Copy-Item .\target\debug\emacs_rs_module.dll .\rs-module.dll
Copy-Item .\target\debug\test_module.dll .\t.dll

emacs --batch --directory . -l ert `
  -l .\test-module\tests\main.el `
  -f ert-run-tests-batch-and-exit
