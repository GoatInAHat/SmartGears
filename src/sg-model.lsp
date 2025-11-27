;;; sg-model.lsp -- gear data structures and validation for SmartGears
;;; Purpose: build association lists representing external spur gears.
;;; Side effects: none (pure data preparation)
;;; Units: millimeters

(defun sg:model-validate-inputs (z module pressure bore kerf)
  "Validate numeric inputs and return T if valid."
  (and (numberp z) (> z 4)
       (numberp module) (> module 0.0)
       (numberp pressure) (> pressure 0.0)
       (numberp bore) (>= bore 0.0)
       (numberp kerf)))

(defun sg:gear-make (z module pressure bore kerf center)
  "Create a gear data structure (alist) containing parameters, derived radii, and outline points.
Relies on sg-math pure helpers; no AutoCAD calls here."
  (if (not (sg:model-validate-inputs z module pressure bore kerf))
    (progn (princ "Invalid gear parameters.\n") nil)
    (sg:gear->alist z module pressure bore kerf center)))

(princ)
