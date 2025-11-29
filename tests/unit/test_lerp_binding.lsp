;;; test_lerp_binding.lsp -- Regression test for sg-lerp not binding the constant T.
;;; Ensures linear interpolation runs without attempting to rebind AutoLISP's true symbol.

(defun sg--load-bundle ( / path)
  (cond
    ((findfile "SmartGears.lsp") (setq path "SmartGears.lsp"))
    ((findfile "../SmartGears.lsp") (setq path "../SmartGears.lsp")))
  (if path (load path)))

(defun sg-test-lerp-no-const-bind ( / res val)
  (sg--load-bundle)
  (setq val (sg-lerp 0 10 0.5))
  (if (and val (= val 5.0))
      'pass
      (list 'fail 'lerp-value val)))

(princ)
