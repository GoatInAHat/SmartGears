;;; math-angle.lsp -- Angle normalization and conversions (radians-only API target).
;;; Purpose: normalize wrapping, degree/radian conversions, toleranced comparisons.

(defun sg-angle-normalize (theta / twopi res)
  "Wrap THETA into [0, 2*pi)."
  (setq twopi (* 2 *sg-pi*))
  (setq res (rem theta twopi))
  (if (< res 0) (setq res (+ res twopi)))
  res)

(defun sg-angle-wrap-pi (theta / res)
  "Wrap THETA into (-pi, pi]."
  (setq res (sg-angle-normalize theta))
  (if (> res *sg-pi*)
      (- res (* 2 *sg-pi*))
      res))

(defun sg-deg->rad (deg)
  "Convert degrees to radians."
  (* deg (/ *sg-pi* 180.0)))

(defun sg-rad->deg (rad)
  "Convert radians to degrees."
  (/ (* rad 180.0) *sg-pi*))

(defun sg-angle-diff (a b)
  "Minimal signed angular difference from A to B in (-pi, pi]."
  (sg-angle-wrap-pi (- b a)))

;; Aliases for spec wording
(defun normalize-angle (theta) (sg-angle-normalize theta))
(defun wrap-pi (theta) (sg-angle-wrap-pi theta))
(defun deg->rad (deg) (sg-deg->rad deg))
(defun rad->deg (rad) (sg-rad->deg rad))
(defun angle-diff (a b) (sg-angle-diff a b))

(princ)
