;;; sg-load.lsp -- loader and command entry points for SmartGears
;;; Purpose: orchestrate module loading and register public commands.

(defun sg:prompt-real (msg default)
  "Prompt for a real number with DEFAULT fallback."
  (initget 6)
  (let ((val (getreal (strcat msg " <" (rtos default 2 3) ">: "))))
    (if val val default)))

(defun sg:prompt-int (msg default)
  "Prompt for an integer with DEFAULT fallback."
  (initget 6)
  (let ((val (getint (strcat msg " <" (itoa default) ">: "))))
    (if val val default)))

(defun c:SGEARMAKE ()
  "Create an external spur gear using SmartGears involute geometry."
  (princ "\n-- SmartGears: External Spur Gear --")
  (let* ((z (sg:prompt-int "\nNumber of teeth" 24))
         (m (sg:prompt-real "Module (mm)" 2.0))
         (alpha (sg:prompt-real "Pressure angle (deg)" 20.0))
         (bore (sg:prompt-real "Bore diameter (mm)" 5.0))
         (kerf (sg:prompt-real "Kerf / clearance offset (mm, +expand, -shrink)" 0.0))
         (center (getpoint "\nCenter point <0,0,0>: "))
         (origin (if center center '(0 0 0)))
         (gear (sg:gear-make z m alpha bore kerf origin)))
    (if gear
      (sg:acad-draw-gear gear)
      (princ "\nGear generation aborted; invalid inputs.")))
  (princ))

(princ)
