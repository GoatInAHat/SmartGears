;;; test_involute.lsp -- Unit test for involute helpers.

(defun approx= (a b tol) (<= (abs (- a b)) tol))

(defun sg-test-involute ( / module teeth pa pa-rad rp rb p0 p1 p2 phi1)
  (setq module 2.0
        teeth 10
        pa 20.0
        pa-rad (sg-deg->rad pa)
        rp (sg-pitch-radius module teeth)
        rb (sg-base-radius rp pa-rad))

  ;; expected base radius
  (setq ok-rb (approx= rb 9.39693 1e-4))

  ;; involute samples at phi = 0, 0.3, 0.6
  (setq p0 (sg-involute-point rb 0.0))
  (setq p1 (sg-involute-point rb 0.3))
  (setq p2 (sg-involute-point rb 0.6))

  (setq ok-p0 (and (approx= (car p0) 9.39693 1e-4)
                   (approx= (cadr p0) 0.0 1e-5)))
  (setq ok-p1 (and (approx= (car p1) 9.81032 1e-4)
                   (approx= (cadr p1) 0.08381 1e-4)))
  (setq ok-p2 (and (approx= (car p2) 10.93916 1e-4)
                   (approx= (cadr p2) 0.65253 1e-4)))

  ;; invert radius back to phi
  (setq phi1 (sg-involute-angle-from-radius rb (distance '(0 0) p2)))
  (setq ok-phi (approx= phi1 0.6 1e-4))

  (list 'test_involute ok-rb ok-p0 ok-p1 ok-p2 ok-phi))

(princ)
