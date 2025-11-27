;;; sg-load.lsp -- loader and command entry points for SmartGears
;;; Purpose: orchestrate module loading and register public commands.

(defun sg:val-or-default (value default)
  "Return VALUE when numeric; otherwise, DEFAULT.
Uses AutoLISP get* prompts that return NIL on Enter or cancel, so this
provides a safe fallback for both interactive and programmatic entry.
Refs: Autodesk community guidance shows getint/getreal return NIL on
empty Enter, enabling manual defaults. https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/getint-how-to-allow-user-to-press-enter-to-use-default/td-p/5630575
Additional examples echo the same pattern with getreal returning NIL when the user presses Enter. https://www.cadtutor.net/forum/topic/3755-exiting-getreal-if-enter-is-hit/"
  (if (numberp value) value default))

(defun VAL (value default)
  "Backward-compatible alias for sg:val-or-default."
  (sg:val-or-default value default))

(defun sg:prompt-real (msg default flags)
  "Prompt for a real number with DEFAULT fallback and optional INITGET FLAGS."
  (if flags (initget flags) (initget))
  (let ((val (getreal (strcat msg " <" (rtos default 2 3) ">: "))))
    (sg:val-or-default val default)))

(defun sg:prompt-int (msg default flags)
  "Prompt for an integer with DEFAULT fallback and optional INITGET FLAGS."
  (if flags (initget flags) (initget))
  (let ((val (getint (strcat msg " <" (itoa default) ">: "))))
    (sg:val-or-default val default)))

(defun sg:plist-number (params key default)
  "Return numeric value from PARAMS plist by KEY, falling back to DEFAULT."
  (sg:val-or-default (cdr (assoc key params)) default))

(defun SGEARMAKE (params)
  "Create an external spur gear using SmartGears involute geometry.

When PARAMS is NIL, prompts interactively using AutoCAD command-line
inputs. When PARAMS is a property list (e.g., '(:module 2 :teeth 24 :pressure-angle 20 :bore 5 :kerf 0.0)), values are taken
directly without prompting so CI and automation can exercise the
geometry pipeline."
  (if (not params)
    (princ "\n-- SmartGears: External Spur Gear --"))
  (let* ((z (if params
              (sg:plist-number params :teeth 24)
              (sg:prompt-int "\nNumber of teeth" 24 6)))
         (m (if params
              (sg:plist-number params :module 2.0)
              (sg:prompt-real "Module (mm)" 2.0 6)))
         (alpha (if params
                 (sg:plist-number params :pressure-angle 20.0)
                 (sg:prompt-real "Pressure angle (deg)" 20.0 6)))
         (bore (if params
                (sg:plist-number params :bore 5.0)
                (sg:prompt-real "Bore diameter (mm)" 5.0 4)))
         (kerf (if params
                (sg:plist-number params :kerf 0.0)
                (sg:prompt-real "Kerf / clearance offset (mm, +expand, -shrink)" 0.0 nil)))
         (center (if params (cdr (assoc :center params)) (getpoint "\nCenter point <0,0,0>: ")))
         (origin (if center center '(0 0 0)))
         (gear (sg:gear-make z m alpha bore kerf origin)))
    (if gear
      (sg:acad-draw-gear gear)
      (princ "\nGear generation aborted; invalid inputs.")))
  (princ))

(defun c:SGEARMAKE () (SGEARMAKE nil))

(princ)
