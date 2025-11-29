;;; acad-layer.lsp -- Layer helpers for SmartGears.
;;; Purpose: shared layer creation/check for commands.

(defun sg-ensure-layer (name / existing created)
  "Ensure layer NAME exists; return T if available."
  (setq existing (tblsearch "LAYER" name))
  (cond
    (existing T)
    ((setq created (entmake (list '(0 . "LAYER")
                                  (cons 2 name)
                                  '(70 . 0)
                                  '(62 . 7)
                                  '(6 . "CONTINUOUS"))))
     T)
    ((progn
       (command "._-LAYER" "_New" name "")
       (tblsearch "LAYER" name)))
    (T nil)))

;; Alias used by earlier modules
(defun ensure-layer (name) (sg-ensure-layer name))

(princ)
