;;; test_sgmvp_geom.lsp -- Compare SGMVP geometry to direct generator output.

(defun approx= (a b tol) (<= (abs (- a b)) tol))

(defun sg--load-bundle ( / path)
  (cond
    ((findfile "SmartGears.lsp") (setq path "SmartGears.lsp"))
    ((findfile "../SmartGears.lsp") (setq path "../SmartGears.lsp")))
  (if path (load path)))

(defun sg-test-sgmvp-geom ( / res geom1 geom2 pts1 pts2)
  (sg--load-bundle)
  (setq res (sg-mvp-run T))
  (setq geom1 (cdr (assoc 'geom res)))
  (setq geom2 (sg-generate-spur-gear-geom (sg-mvp-params)))
  (setq pts1 (cdr (assoc 'points geom1)))
  (setq pts2 (cdr (assoc 'points geom2)))
  (if (and (= (length pts1) (length pts2))
           (approx= (car (car pts1)) (car (car pts2)) 1e-6)
           (approx= (cadr (car pts1)) (cadr (car pts2)) 1e-6)
           (approx= (cdr (assoc 'pitch-radius geom1)) (cdr (assoc 'pitch-radius geom2)) 1e-6)
           (approx= (cdr (assoc 'base-radius geom1)) (cdr (assoc 'base-radius geom2)) 1e-6))
      'pass
      (list 'fail 'geom-mismatch)))

(princ)
