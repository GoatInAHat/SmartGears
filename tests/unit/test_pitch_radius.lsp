;;; test_pitch_radius.lsp -- Unit test for spur pitch radius calculation.

(defun approx= (a b tol) (<= (abs (- a b)) tol))

(defun sg-test-pr-radius ( / r)
  (setq r (sg-spur-pitch-radius 2.0 20))
  (if (approx= r 20.0 1e-6) 'pass (list 'fail "pitch-radius" r)))

(princ)
