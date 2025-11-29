;;; geom-spur.lsp -- Spur gear profile generation (external/internal) scaffolding.
;;; Purpose: compute nominal radii, assemble tooth outlines, return closed polygons.

;; ---- helpers ----
(defun sg-spur-pitch-radius (module teeth)
  (* module 0.5 teeth))

(defun sg-spur-base-radius (module teeth pressure-angle)
  (sg-base-radius (sg-spur-pitch-radius module teeth) pressure-angle))

(defun sg--rot-point (pt ang)
  (vec-rotate pt ang))

(defun sg--mirror-x (pt)
  ;; Mirror across the X-axis (flip Y) to create the opposite flank in local coords.
  (list (car pt) (- (cadr pt))))

(defun sg--polar (r ang)
  (list (* r (cos ang)) (* r (sin ang))))

(defun sg--close-loop (pts)
  (if (equal (car pts) (car (last pts)) 1e-6)
      pts
      (append pts (list (car pts)))))

;; ---- main generator ----
(defun sg-generate-spur-gear-geom (gear / module teeth pa_rad inv-pa rp rb ra rr
                                        add ded half-thick tooth-ang flank-samples
                                        tooth-index pts r-start r-end flank
                                        flank-right flank-left tooth-pts
                                        next-base root-start-angle root-end-angle
                                        root-mid root-arc)
  ;; gear alist expected keys: module teeth pa(deg)
  (setq module (cdr (assoc 'module gear)))
  (setq teeth (cdr (assoc 'teeth gear)))
  (setq pa_rad (sg-deg->rad (cdr (assoc 'pa gear))))
  ;; AutoLISP lacks TAN; use sin/cos to derive inv(pressure-angle) = tan(pa) - pa
  (setq inv-pa (- (/ (sin pa_rad) (cos pa_rad)) pa_rad))
  (setq rp (sg-spur-pitch-radius module teeth))
  (setq rb (sg-spur-base-radius module teeth pa_rad))
  (setq add module)
  (setq ded (* 1.25 module))
  (setq ra (+ rp add))
  (setq rr (max 0.001 (- rp ded))) ;; avoid zero/negative
  (setq half-thick (/ *sg-pi* (* 2 teeth))) ;; half tooth thickness at pitch circle
  (setq tooth-ang (/ (* 2 *sg-pi*) teeth))
  (setq flank-samples 24)

  ;; flank points from root (clipped to base) to tip on one side
  (setq pts '())
  (setq tooth-index 0)
  (while (< tooth-index teeth)
    (setq base-angle (* tooth-index tooth-ang))

    ;; radii bounds for involute sampling
    (setq r-start (max rb rr))
    (setq r-end ra)
    (setq flank (sg-involute-flank rb r-start r-end flank-samples))

    ;; Apply pressure-angle taper: right flank mirrors across X, left remains unmirrored.
    ;; Rotation includes the involute function term (tan(pa)-pa) to keep addendum narrower.
    (setq flank-right (mapcar '(lambda (p)
                                 (sg--rot-point (sg--mirror-x p)
                                               (+ base-angle half-thick inv-pa)))
                               flank))
    (setq flank-left (mapcar '(lambda (p)
                                (sg--rot-point p (- base-angle half-thick inv-pa)))
                              flank))

    ;; order: left root->tip, right tip->root
    (setq tooth-pts (append flank-left (reverse flank-right)))

    ;; root connection to next tooth (approximate circular arc at root radius)
    (setq next-base (+ base-angle tooth-ang))
    ;; Root arc uses nominal half-thickness positions to stitch between teeth.
    (setq root-start-angle (angle '(0.0 0.0) (car flank-right)))
    (setq root-end-angle (+ base-angle tooth-ang (- half-thick))) ;; next tooth left root
    (setq root-mid (sg-lerp root-start-angle root-end-angle 0.5))
    (setq root-arc (list
                     (sg--polar rr root-start-angle)
                     (sg--polar rr root-mid)
                     (sg--polar rr root-end-angle)))

    ;; For clean stitching, append tooth points and arc to next tooth start
    (setq pts (append pts tooth-pts root-arc))
    (setq tooth-index (1+ tooth-index)))

  ;; Close loop
  (setq pts (sg--close-loop pts))

  (list
    (cons 'points pts)
    (cons 'pitch-radius rp)
    (cons 'base-radius rb)
    (cons 'outer-radius ra)
    (cons 'root-radius rr)))

;; Backward compatibility wrapper name
(defun sg-spur-outline (gear-params)
  (sg-generate-spur-gear-geom gear-params))

(princ)
