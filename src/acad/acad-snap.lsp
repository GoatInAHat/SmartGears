;;; acad-snap.lsp -- Placement/snap helpers for gear alignment (Phase 0 stubs).
;;; Purpose: center-distance calculations, MOVE/ROTATE command wrappers, phase alignment.

(defun sg-snap-center-distance (gear-a gear-b clearance / rp-a rp-b)
  "Compute nominal center distance between GEAR-A and GEAR-B with CLEARANCE."
  (setq rp-a (cdr (assoc 'pitch-radius gear-a)))
  (setq rp-b (cdr (assoc 'pitch-radius gear-b)))
  (if (and rp-a rp-b)
      (+ rp-a rp-b clearance)
      nil))

(defun sg-snap-align-phase (gear-a gear-b)
  "Placeholder for future phase alignment; returns placeholder sentinel."
  *sg-placeholder*)

(princ)
