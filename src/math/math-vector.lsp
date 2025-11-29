;;; math-vector.lsp -- Basic 2D vector operations.
;;; Purpose: vector add/subtract/scale/dot helpers for pure geometry math.

(defun sg-v-add (a b)
  "Add vectors A and B (2D or 3D lists)."
  (mapcar '+ a b))

(defun sg-v-sub (a b)
  "Subtract B from A."
  (mapcar '- a b))

(defun sg-v-scale (v s)
  "Scale vector V by scalar S."
  (mapcar '(lambda (c) (* c s)) v))

(defun sg-v-dot (a b)
  "Dot product of vectors A and B."
  (apply '+ (mapcar '* a b)))

(defun sg-v-length (v)
  "Euclidean length of vector V."
  (sqrt (apply '+ (mapcar '(lambda (c) (* c c)) v))))

(defun sg-v-angle-between (a b / denom cosv)
  "Angle between vectors A and B (radians). Returns nil if a or b is zero."
  (setq denom (* (sg-v-length a) (sg-v-length b)))
  (if (and denom (> denom 1e-9))
      (progn
        (setq cosv (/ (sg-v-dot a b) denom))
        (setq cosv (sg-clamp cosv -1.0 1.0))
        (acos cosv))
      nil))

(defun sg-v-rotate (v ang / c s x y)
  "Rotate 2D vector V by ANG radians."
  (setq c (cos ang)
        s (sin ang)
        x (car v)
        y (cadr v))
  (list (- (* x c) (* y s)) (+ (* x s) (* y c))))

;; Aliases matching spec phrasing
(defun vec-add (a b) (sg-v-add a b))
(defun vec-sub (a b) (sg-v-sub a b))
(defun vec-scale (v s) (sg-v-scale v s))
(defun vec-length (v) (sg-v-length v))
(defun vec-angle-between (a b) (sg-v-angle-between a b))
(defun vec-rotate (v ang) (sg-v-rotate v ang))

(princ)
