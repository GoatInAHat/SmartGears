;;; sg-acad.lsp -- AutoCAD integration for SmartGears
;;; Purpose: draw computed gear geometry using AutoCAD-compatible commands (desktop + Web)
;;; Avoids ActiveX/VLAX/VLA for Web compatibility.

(defun sg:acad-make-polyline (pts)
  "Create a closed lightweight polyline from PTS (list of (x y z))."
  (if (and pts (> (length pts) 2))
    (entmake
      (append
        '((0 . "LWPOLYLINE") (100 . "AcDbEntity") (100 . "AcDbPolyline"))
        (list (cons 90 (length pts)) '(70 . 1))
        (apply 'append (mapcar (function (lambda (p) (list (cons 10 (car p)) (cons 20 (cadr p)) (cons 30 (if (caddr p) (caddr p) 0.0))))) pts))))
    (princ "Polyline skipped; not enough points.")))

(defun sg:acad-draw-gear (gear)
  "Draw the full gear outline, root circle, outer circle, and bore (if > 0)."
  (let* ((outline (cdr (assoc 'outline gear)))
         (center (cdr (assoc 'center gear)))
         (tip (cdr (assoc 'tip-radius gear)))
         (root (cdr (assoc 'root-radius gear)))
         (bore (cdr (assoc 'bore gear))))
    (if outline (sg:acad-make-polyline outline))
    (if tip (command "_.CIRCLE" center tip))
    (if root (command "_.CIRCLE" center root))
    (if (and bore (> bore 0.0)) (command "_.CIRCLE" center (* 0.5 bore)))
    (princ "\nSmartGears gear drawn.")))

(princ)
