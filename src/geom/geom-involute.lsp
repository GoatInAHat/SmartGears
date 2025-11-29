;;; geom-involute.lsp -- Involute curve helpers for spur gear flanks.
;;; Purpose: base-circle parametrization, flank sampling, root fillet placeholders.

;; Base circle radius from pitch radius and pressure angle (radians)
(defun sg-base-radius (pitch-radius pressure-angle)
  (* pitch-radius (cos pressure-angle)))

;; Pitch radius helper (module & teeth)
(defun sg-pitch-radius (module teeth)
  (* module 0.5 teeth))

(defun sg-involute-point (base-radius phi / cosphi sinphi)
  "Return 2D point on the involute from BASE-RADIUS at parameter PHI (radians)."
  (setq cosphi (cos phi)
        sinphi (sin phi))
  (list
    (* base-radius (+ cosphi (* phi sinphi)))
    (* base-radius (- sinphi (* phi cosphi)))))

(defun sg-involute-angle-from-radius (base-radius target-radius / ratio)
  "Solve for involute parameter PHI such that radius(target) = target-radius."
  (if (< base-radius 1e-9)
      nil
      (progn
        (setq ratio (/ target-radius base-radius))
        (if (< ratio 1.0)
            nil
            (sqrt (- (* ratio ratio) 1.0))))))

(defun sg-involute-flank (base-radius r-start r-end samples / phi0 phi1 step pts i phi)
  "Sample involute flank from R-START to R-END (inclusive) with SAMPLES count."
  (setq phi0 (sg-involute-angle-from-radius base-radius r-start))
  (setq phi1 (sg-involute-angle-from-radius base-radius r-end))
  (if (or (null phi0) (null phi1) (<= samples 1))
      nil
      (progn
        (setq step (/ (- phi1 phi0) (1- samples)))
        (setq pts '()
              i 0)
        (while (< i samples)
          (setq phi (+ phi0 (* i step)))
          (setq pts (append pts (list (sg-involute-point base-radius phi))))
          (setq i (1+ i)))
        pts)))

;; Root fillet placeholder: caller may blend later
(defun sg-involute-root-fillet (root-radius start-angle end-angle / mid)
  "Return three-point arc approximation on ROOT-RADIUS from START-ANGLE to END-ANGLE."
  (setq mid (sg-lerp start-angle end-angle 0.5))
  (list
    (sg--polar root-radius start-angle)
    (sg--polar root-radius mid)
    (sg--polar root-radius end-angle)))

(defun sg--polar (r ang)
  (list (* r (cos ang)) (* r (sin ang))))

;; Aliases per spec wording
(defun involute-point (base-radius phi) (sg-involute-point base-radius phi))
(defun involute-angle-from-radius (base-radius target-radius)
  (sg-involute-angle-from-radius base-radius target-radius))
(defun compute-base-circle (pitch-radius pressure-angle)
  (sg-base-radius pitch-radius pressure-angle))
(defun compute-pitch-circle (module teeth) (sg-pitch-radius module teeth))

(princ)
