;;; sg-input-tests.lsp -- unit tests for SmartGears input helpers
;;; Purpose: validate sg:val-or-default fallback behavior for numeric inputs.
;;; Usage: load after SmartGears.lsp and run (sg:run-input-tests)
;;; Notes: designed for AutoCAD or compatible AutoLISP runtime; mocks are
;;; unnecessary because sg:val-or-default is pure.

(setq sg:input-tests-failed 0)

(defun sg:test-equal (label expected actual)
  "Simple equality assertion for test harness."
  (if (= expected actual)
    (princ (strcat "\nPASS: " label))
    (progn
      (setq sg:input-tests-failed (1+ sg:input-tests-failed))
      (princ (strcat "\nFAIL: " label " expected " (vl-princ-to-string expected) " got " (vl-princ-to-string actual))))))

(defun sg:run-input-tests ()
  "Execute sg:val-or-default unit tests and report pass/fail counts."
  (setq sg:input-tests-failed 0)
  (sg:test-equal "numeric is returned" 5 (sg:val-or-default 5 2))
  (sg:test-equal "NIL falls back" 2 (sg:val-or-default nil 2))
  (sg:test-equal "non-numeric falls back" 2 (sg:val-or-default "text" 2))
  (if (= 0 sg:input-tests-failed)
    (princ "\nsg:val-or-default tests passed." )
    (princ (strcat "\nsg:val-or-default failures: " (itoa sg:input-tests-failed))))
  (princ))

(princ)
