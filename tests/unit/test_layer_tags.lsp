;;; test_layer_tags.lsp -- Verify DXF builders carry explicit layer tags.

(defun sg--load-bundle ( / path)
  (cond
    ((findfile "SmartGears.lsp") (setq path "SmartGears.lsp"))
    ((findfile "../SmartGears.lsp") (setq path "../SmartGears.lsp")))
  (if path (load path)))

(defun sg-test-layer-tags ( / pts poly-dxf circle-dxf layer-tag circle-layer)
  (sg--load-bundle)
  (setq pts '((0.0 0.0) (1.0 0.0) (1.0 1.0)))
  (setq poly-dxf (sg--polyline-dxf pts "SGEARS"))
  (setq circle-dxf (sg--circle-dxf '(0.0 0.0) 1.0 "SGEARS"))
  (setq layer-tag (assoc 8 poly-dxf))
  (setq circle-layer (assoc 8 circle-dxf))
  (if (and layer-tag circle-layer
           (equal (cdr layer-tag) "SGEARS")
           (equal (cdr circle-layer) "SGEARS"))
      'pass
      (list 'fail 'missing-layer layer-tag circle-layer)))

(princ)
