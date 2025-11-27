;;; sg-test.lsp -- simple load verification command for SmartGears
;;; Purpose: draw a minimal test entity to validate command wiring.
;;; Inputs: none
;;; Outputs: creates a line entity from (0,0,0) to (10,0,0)
;;; Side effects: draws in current drawing

(defun c:SGEAR_TEST ()
  "Draw a simple line to confirm SmartGears test harness wiring."
  (command "_.LINE" '(0 0 0) '(10 0 0) "")
  (princ "\nSGEAR_TEST executed; line created."))

(princ)
