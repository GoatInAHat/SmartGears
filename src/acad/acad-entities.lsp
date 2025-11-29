;;; acad-entities.lsp -- AutoCAD entity builders (entmake) scaffolding.
;;; Purpose: wrappers for lightweight polyline and circle creation; AutoCAD-only.
;;; Reference: entmake_AutoLISP.md, entmakex_AutoLISP.md, FAQ lightweight polylines.

(defun sg-ensure-closed-polyline (points / pfirst plast)
  "Ensure first and last vertices are equal."
  (setq pfirst (car points)
        plast (car (last points)))
  (if (equal pfirst plast 1e-6)
      points
      (append points (list pfirst))))

(defun sg--polyline-dxf (points layer / pts n dxf)
  "Build the DXF list for a closed LWPOLYLINE; optional LAYER tag."
  (setq pts (sg-ensure-closed-polyline points))
  (setq n (length pts))
  (setq dxf (list
              '(0 . "LWPOLYLINE")
              '(100 . "AcDbEntity")
              '(100 . "AcDbPolyline")
              (cons 90 n)
              '(70 . 1))) ;; closed flag
  (if layer (setq dxf (append dxf (list (cons 8 layer)))))
  (foreach p pts
    (setq dxf (append dxf (list (cons 10 (list (car p) (cadr p))))))) ;; bulge defaults to 0
  dxf)

(defun sg--circle-dxf (center radius layer / dxf)
  "Build the DXF list for a circle; optional LAYER tag."
  (setq dxf (list
              '(0 . "CIRCLE")
              (cons 10 (list (car center) (cadr center) 0.0))
              (cons 40 radius)))
  (if layer (setq dxf (append dxf (list (cons 8 layer)))))
  dxf)

(defun sg-entmake-polyline (points layer / )
  "Create a closed lightweight polyline via entmake. Optional LAYER overrides CLAYER."
  (entmake (sg--polyline-dxf points layer)))

(defun sg-entmake-circle (center radius layer / )
  "Create a circle via entmake. Optional LAYER overrides CLAYER."
  (entmake (sg--circle-dxf center radius layer)))

;; Aliases requested
(defun acad-make-polyline (points layer) (sg-entmake-polyline points layer))
(defun acad-make-circle (center radius layer) (sg-entmake-circle center radius layer))

(princ)
