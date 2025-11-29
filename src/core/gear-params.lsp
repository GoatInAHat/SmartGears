;;; gear-params.lsp -- Gear parameter defaults, tolerance presets, and ID helpers.
;;; Purpose: shared, CAD-free utilities for commands needing presets or IDs.

;; -------------------------
;; Gear ID generator
;; -------------------------
(setq *sg-gear-id-counters* '())

(defun sg-next-gear-id (prefix / rec count)
  "Return monotonically increasing gear id string with PREFIX (e.g., SG-1)."
  (setq rec (assoc prefix *sg-gear-id-counters*))
  (if rec
      (progn
        (setq count (1+ (cdr rec)))
        (setq *sg-gear-id-counters* (subst (cons prefix count) rec *sg-gear-id-counters*)))
      (progn
        (setq count 1)
        (setq *sg-gear-id-counters* (append *sg-gear-id-counters* (list (cons prefix count))))))
  (strcat prefix "-" (rtos count 2 0)))

;; -------------------------
;; Defaults
;; -------------------------
(defun sg-spur-default-module (teeth)
  "Heuristic default module. Keeps small gears sturdier; falls back to 2.0."
  (cond
    ((<= teeth 10) 3.0)
    ((<= teeth 14) 2.5)
    (T 2.0)))

(defun sg-spur-default-bore (module teeth / min-bore scaled)
  "Scale bore with pitch diameter while keeping a 5 mm minimum."
  (setq min-bore 5.0)
  (setq scaled (* 0.2 module teeth)) ;; ~20% of pitch diameter
  (max min-bore scaled))

;; -------------------------
;; Tolerance presets
;; -------------------------
(setq *sg-tolerance-presets*
  (list
    (cons "NONE" (list (cons 'name "NONE")
                       (cons 'kerf 0.0)
                       (cons 'backlash 0.0)
                       (cons 'bore_oversize 0.0)
                       (cons 'notes "No kerf/backlash offsets.")))
    (cons "MDF3" (list (cons 'name "MDF3")
                       (cons 'kerf 0.15)
                       (cons 'backlash 0.07)
                       (cons 'bore_oversize 0.15)
                       (cons 'notes "MDF 3mm laser cut.")))
    (cons "MDF6" (list (cons 'name "MDF6")
                       (cons 'kerf 0.20)
                       (cons 'backlash 0.10)
                       (cons 'bore_oversize 0.20)
                       (cons 'notes "MDF 6.4mm laser cut.")))
    (cons "PLY6" (list (cons 'name "PLY6")
                       (cons 'kerf 0.18)
                       (cons 'backlash 0.10)
                       (cons 'bore_oversize 0.18)
                       (cons 'notes "6mm birch ply.")))
    (cons "ACRYLIC3" (list (cons 'name "ACRYLIC3")
                           (cons 'kerf 0.12)
                           (cons 'backlash 0.05)
                           (cons 'bore_oversize 0.10)
                           (cons 'notes "Cast acrylic 3mm.")))
    (cons "PLA-FFF" (list (cons 'name "PLA-FFF")
                          (cons 'kerf 0.0)
                          (cons 'backlash 0.08)
                          (cons 'bore_oversize 0.15)
                          (cons 'notes "FFF print, 0.4mm nozzle.")))))

(setq *sg-active-tolerance* (cdr (assoc "NONE" *sg-tolerance-presets*)))

(defun sg--find-tolerance (name / target found)
  (setq target (strcase name))
  (setq found nil)
  (foreach entry *sg-tolerance-presets*
    (if (= (strcase (car entry)) target)
        (setq found (cdr entry))))
  found)

(defun sg-get-tolerance-preset (name)
  "Lookup tolerance preset by NAME (case-insensitive). Returns alist or nil."
  (if name (sg--find-tolerance name) nil))

(defun sg-active-tolerance ( / )
  "Return currently active tolerance preset."
  *sg-active-tolerance*)

(defun sg-set-active-tolerance (name / preset)
  "Set active tolerance preset; falls back to NONE when missing."
  (setq preset (sg-get-tolerance-preset name))
  (if preset
      (setq *sg-active-tolerance* preset)
      (setq *sg-active-tolerance* (cdr (assoc "NONE" *sg-tolerance-presets*))))
  *sg-active-tolerance*)

(defun sg-tolerance-names ( / names)
  "Return list of available tolerance preset names."
  (setq names '())
  (foreach entry *sg-tolerance-presets*
    (setq names (append names (list (car entry)))))
  names)

;; Aliases for other modules
(defun spur-default-module (teeth) (sg-spur-default-module teeth))
(defun spur-default-bore (module teeth) (sg-spur-default-bore module teeth))
(defun get-tolerance-preset (name) (sg-get-tolerance-preset name))
(defun active-tolerance ( ) (sg-active-tolerance))
(defun set-active-tolerance (name) (sg-set-active-tolerance name))

(princ)
