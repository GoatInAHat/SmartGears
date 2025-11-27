;;; sg-math.lsp -- math utilities for SmartGears
;;; Purpose: pure mathematical helpers for involute spur-gear geometry (no AutoCAD dependencies)
;;; Units: millimeters, radians for internal angles
;;; References:
;;; - Involute gear tooth profile basics and formulas: https://www.marplesgears.com/2019/10/an-in-depth-look-at-involute-gear-tooth-profile-and-profile-shift/
;;; - Standard gear geometry (pitch, addendum, dedendum, base circle): https://khkgears.net/new/gear_knowledge/gear_technical_reference/calculation_gear_dimensions.html

(defun sg:deg->rad (deg)
  "Convert degrees to radians."
  (* pi (/ deg 180.0)))

(defun sg:rad->deg (rad)
  "Convert radians to degrees."
  (* 180.0 (/ rad pi)))

(defun sg:cosd (deg)
  "Cosine using degrees."
  (cos (sg:deg->rad deg)))

(defun sg:sind (deg)
  "Sine using degrees."
  (sin (sg:deg->rad deg)))

(defun sg:polar-point (origin dist ang-rad)
  "Return a 2D point at ANG-RAD from ORIGIN with distance DIST."
  (list (+ (car origin) (* dist (cos ang-rad)))
        (+ (cadr origin) (* dist (sin ang-rad)))
        0.0))

(defun sg:rotate-point (pt ang-rad)
  "Rotate PT (x y z) by ANG-RAD about Z."
  (let ((x (car pt))
        (y (cadr pt)))
    (list (- (* x (cos ang-rad)) (* y (sin ang-rad)))
          (+ (* x (sin ang-rad)) (* y (cos ang-rad)))
          (if (caddr pt) (caddr pt) 0.0))))

(defun sg:translate-point (pt offset)
  "Translate PT by OFFSET (dx dy [dz])."
  (list (+ (car pt) (car offset))
        (+ (cadr pt) (cadr offset))
        (+ (if (caddr pt) (caddr pt) 0.0)
           (if (caddr offset) (caddr offset) 0.0))))

(defun sg:mirror-x (pt)
  "Mirror PT across the X-axis (y -> -y)."
  (list (car pt) (- (cadr pt)) (if (caddr pt) (caddr pt) 0.0)))

(defun sg:involute-param (rb target-r)
  "Return involute parameter t such that r = rb * sqrt(1+t^2)."
  (if (<= rb 0.0)
    0.0
    (sqrt (max 0.0 (- (/ (* target-r target-r) (* rb rb)) 1.0)))))

(defun sg:involute-point (rb t)
  "Return a point on an involute defined by base radius RB at parameter T." 
  ;; Parametric form: x = rb*(cos t + t*sin t); y = rb*(sin t - t*cos t)
  (list (* rb (+ (cos t) (* t (sin t))))
        (* rb (- (sin t) (* t (cos t))))
        0.0))

(defun sg:involute-curve (rb rt steps)
  "Return list of involute points from base radius RB to tip radius RT using STEPS subdivisions."
  (if (or (<= steps 0) (<= rb 0.0) (<= rt rb))
    nil
    (let* ((t-max (sg:involute-param rb rt))
           (dt (/ t-max steps))
           (pts nil)
           (i 0))
      (while (<= i steps)
        (setq pts (cons (sg:involute-point rb (* dt i)) pts))
        (setq i (1+ i)))
      (reverse pts))))

(defun sg:arc-points (radius ang-start ang-end steps)
  "Subdivide an arc on RADIUS from ANG-START to ANG-END (radians) into STEPS segments." 
  (if (<= steps 0)
    nil
    (let ((pts nil)
          (i 0)
          (delta (/ (- ang-end ang-start) steps)))
      (while (<= i steps)
        (setq pts (cons (sg:polar-point '(0 0 0) radius (+ ang-start (* delta i))) pts))
        (setq i (1+ i)))
      (reverse pts))))

(defun sg:gear-derived-values (z module pressure-deg bore kerf)
  "Compute derived gear radii for an external spur gear.
Inputs: teeth count Z, MODULE (mm), PRESSURE-DEG (degrees), BORE (diameter, mm), KERF (radial offset mm)."
  (let* ((alpha (sg:deg->rad (if pressure-deg pressure-deg 20.0)))
         (m module)
         (pitch (* 0.5 z m))
         (addendum m)
         (dedendum (* 1.25 m))
         (pitch-r (+ pitch kerf))
         (base-r (+ (* pitch (cos alpha)) kerf))
         (tip-r (+ pitch addendum kerf))
         (root-r (max 0.01 (+ (- pitch dedendum) kerf))))
    (list (cons 'z z)
          (cons 'module m)
          (cons 'pressure pressure-deg)
          (cons 'bore bore)
          (cons 'kerf kerf)
          (cons 'pitch-radius pitch-r)
          (cons 'base-radius base-r)
          (cons 'tip-radius tip-r)
          (cons 'root-radius root-r)
          (cons 'addendum addendum)
          (cons 'dedendum dedendum)
          (cons 'tooth-angle (/ (* 2.0 pi) z))
          (cons 'half-thickness (/ pi (* 2.0 z))))))

(defun sg:build-tooth-outline (derived)
  "Construct the outline (list of points) for a single tooth, starting at left root and ending at the start of the next tooth root arc." 
  (let* ((rb (cdr (assoc 'base-radius derived)))
         (rt (cdr (assoc 'tip-radius derived)))
         (rr (cdr (assoc 'root-radius derived)))
         (tooth-ang (cdr (assoc 'tooth-angle derived)))
         (half-thick (cdr (assoc 'half-thickness derived)))
         (gap-ang (- tooth-ang (* 2.0 half-thick)))
         (involute (sg:involute-curve rb rt 8))
         (left-root (sg:polar-point '(0 0 0) rr (- half-thick)))
         (right-root (sg:polar-point '(0 0 0) rr half-thick))
         (left-flank (mapcar (function (lambda (p) (sg:rotate-point p (- half-thick))))) involute)
         (right-flank (mapcar (function (lambda (p) (sg:rotate-point p half-thick))) involute))
         (outline nil))
    ;; assemble CCW: left root -> left flank (base->tip) -> right flank (tip->base) -> right root -> root arc to next tooth start
    (setq outline (append outline (list left-root)))
    (setq outline (append outline left-flank))
    (setq outline (append outline (reverse right-flank)))
    (setq outline (append outline (list right-root)))
    ;; root arc to next tooth
    (setq outline (append outline (cdr (sg:arc-points rr half-thick (+ half-thick gap-ang) 3))))
    outline))

(defun sg:gear-outline-points (derived)
  "Generate the full gear outline by rotating a single-tooth outline around the gear."
  (let* ((z (cdr (assoc 'z derived)))
         (tooth-ang (cdr (assoc 'tooth-angle derived)))
         (base-outline (sg:build-tooth-outline derived))
         (pts nil)
         (i 0))
    (while (< i z)
      (setq pts (append pts (mapcar (function (lambda (p) (sg:rotate-point p (* i tooth-ang)))) base-outline)))
      (setq i (1+ i)))
    pts))

(defun sg:gear->alist (z module pressure-deg bore kerf center)
  "Return a gear association list containing parameters, derived radii, and outline points centered at CENTER."
  (let* ((derived (sg:gear-derived-values z module pressure-deg bore kerf))
         (outline (sg:gear-outline-points derived))
         (shifted (mapcar (function (lambda (p) (sg:translate-point p center))) outline)))
    (append derived
            (list (cons 'center center)
                  (cons 'outline shifted)))))

(princ)
