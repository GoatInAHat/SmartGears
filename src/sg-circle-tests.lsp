;;; sg-circle-tests.lsp -- tutorial circle commands preserved after removing legacy SGEARMAKE pipeline.
;;; References: AutoCAD AutoLISP Basic Tutorials for custom commands/getint defaults.

(defun c:SGEAR_CIRCLE10 ( / )
  "Draw a 10 mm diameter circle centered at the origin."
  (command "_.CIRCLE" '(0 0 0) 5.0)
  (princ "\nSGEAR_CIRCLE10 drew a 10 mm diameter circle at the origin.")
  (princ))

(defun c:SGEAR_CIRCLEDYN ( / diameter radius)
  "Prompt for a diameter (default 5 mm) and draw the corresponding origin-centered circle."
  (princ "\n-- SmartGears: Circle Diameter Test --")
  (setq diameter (getint "\nEnter circle diameter in mm <5>: "))
  (if (not (numberp diameter)) (setq diameter 5))
  (setq radius (/ diameter 2.0))
  (command "_.CIRCLE" '(0 0 0) radius)
  (princ (strcat "\nSGEAR_CIRCLEDYN drew a " (itoa diameter) " mm circle at the origin."))
  (princ))

(princ)
