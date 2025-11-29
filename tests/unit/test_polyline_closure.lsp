;;; test_polyline_closure.lsp -- Unit test for spur outline closure.

(defun approx= (a b tol) (<= (abs (- a b)) tol))

(defun sg-test-polyline-closure ( / gear outline pts first last)
  (setq gear (list (cons 'module 2.0) (cons 'teeth 10) (cons 'pa 20.0)))
  (setq outline (sg-generate-spur-gear-geom gear))
  (setq pts (cdr (assoc 'points outline)))
  (setq first (car pts))
  (setq last (car (last pts)))
  (if (and (approx= (car first) (car last) 1e-4)
           (approx= (cadr first) (cadr last) 1e-4))
      'pass
      (list 'fail "not-closed" first last)))

(princ)
