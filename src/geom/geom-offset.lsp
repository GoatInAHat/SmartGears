;;; geom-offset.lsp -- Kerf/backlash offset helpers for tooth geometry.
;;; Purpose: radial offsets, angular thinning, bore oversize adjustments.

(defun sg-apply-kerf-backlash (points kerf backlash pitch-radius / delta-r delta-theta)
  "Apply kerf/backlash thinning to POINTS in polar space relative to origin."
  (setq delta-r (+ (/ kerf 2.0) (/ backlash 2.0)))
  (setq delta-theta (if (and pitch-radius (> pitch-radius 1e-9))
                        (/ backlash pitch-radius)
                        0.0))
  (mapcar
    '(lambda (pt)
       (let* ((r (distance '(0 0) pt))
              (ang (angle '(0 0) pt))
              (sign (if (>= (sin ang) 0.0) 1.0 -1.0))
              (new-r (max 0.0 (- r delta-r)))
              (new-ang (+ ang (* sign 0.5 delta-theta))))
         (list (* new-r (cos new-ang)) (* new-r (sin new-ang)))))
    points))

(defun sg-offset-bore (bore-diam oversize)
  "Return adjusted bore diameter with oversize allowance."
  (+ bore-diam oversize))

;; Legacy stub names retained for compatibility
(defun apply-kerf-backlash (points kerf backlash pitch-radius)
  (sg-apply-kerf-backlash points kerf backlash pitch-radius))

(princ)
