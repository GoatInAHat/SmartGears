;;; math-util.lsp -- Scalar helpers and shared placeholder values for SmartGears.
;;; Purpose: numeric guard rails (clamp, interpolation), stable hash helper, shared sentinels.

(setq *sg-placeholder* :sg-placeholder)
(setq *sg-pi* (* 4 (atan 1.0))) ;; portable pi constant

(defun sg-placeholder ( / )
  "Return the placeholder sentinel used by unimplemented scaffolding paths."
  *sg-placeholder*)

(defun sg-ensure-number (value default)
  "Coerce VALUE to a real number or fall back to DEFAULT when not numberp."
  (if (numberp value) value default))

(defun sg-clamp (val minv maxv)
  "Clamp VAL into the inclusive range [MINV, MAXV]."
  (cond ((< val minv) minv)
        ((> val maxv) maxv)
        (T val)))

(defun sg-lerp (a b u)
  "Linear interpolation between A and B using fraction U in [0,1]."
  (+ a (* (- b a) u)))

(defun sg-safe-div (num denom / eps)
  "Safe division with zero guard; returns nil on zero denominator."
  (setq eps 1e-9)
  (if (and denom (not (= denom 0.0)) (> (abs denom) eps))
      (/ num denom)
      nil))

;; Simple stable hash (FNV-1a–style, 32-bit) for lists/atoms to allow quick regression checks.
(defun sg-crc-hash-list (lst / acc)
  (setq acc 2166136261) ;; FNV offset basis
  (foreach item lst
    (setq acc (sg--crc-update acc item)))
  acc)

(defun sg--crc-update (acc item / val)
  (cond
    ((numberp item)
     (setq val (fix (* 1000000.0 (abs item))))) ; scale to reduce float drift
    ((stringp item)
     (setq val (sg--crc-string item)))
    ((listp item)
     (setq val (sg-crc-hash-list item)))
    (T (setq val 0)))
  ;; FNV-1a: acc = (acc XOR val) * prime (mod 2^32)
  (setq acc (logxor acc val))
  (setq acc (logand (* acc 16777619) 4294967295))
  acc)

(defun sg--crc-string (s / acc i ch)
  (setq acc 0
        i 1)
  (while (<= i (strlen s))
    (setq ch (ascii (substr s i 1)))
    (setq acc (logand (* (+ acc ch) 1315423911) 4294967295))
    (setq i (1+ i)))
  acc)

;; Aliases requested by spec wording
(defun clamp (val minv maxv) (sg-clamp val minv maxv))
(defun lerp (a b u) (sg-lerp a b u))
(defun crc-hash-list (lst) (sg-crc-hash-list lst))

(princ)
