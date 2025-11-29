;;; test_sgmvp_load.lsp -- Ensure SGMVP loads and mock run resolves symbols.

(defun sg--load-bundle ( / path)
  (cond
    ((findfile "SmartGears.lsp") (setq path "SmartGears.lsp"))
    ((findfile "../SmartGears.lsp") (setq path "../SmartGears.lsp")))
  (if path (load path)))

(defun sg-test-sgmvp-load ( / res)
  (sg--load-bundle)
  (setq res (sg-mvp-run T))
  (if res 'pass (list 'fail 'sgmvp-error)))

(princ)
