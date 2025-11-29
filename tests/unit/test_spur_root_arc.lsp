;;; test_spur_root_arc.lsp -- Ensure spur root arc flows forward (no backward jump).
;;; Focuses on the first tooth of the default SGMVP gear to catch ordering regressions.

(defun sg--load-bundle ( / path)
  (cond
    ((findfile "SmartGears.lsp") (setq path "SmartGears.lsp"))
    ((findfile "../SmartGears.lsp") (setq path "../SmartGears.lsp")))
  (if path (load path)))

(defun sg--point->3d (p)
  (list (car p) (cadr p) 0.0))

(defun sg-test-spur-root-arc-order ( / gear geom pts right-root root-start ang1 ang2 delta)
  (sg--load-bundle)
  (setq gear (sg-mvp-params))
  (setq geom (sg-generate-spur-gear-geom gear))
  (setq pts (cdr (assoc 'points geom)))
  ;; With flank-samples = 24, the first tooth contributes 48 flank points.
  (setq right-root (nth 47 pts))
  (setq root-start (nth 48 pts))
  (setq ang1 (angle '(0.0 0.0 0.0) (sg--point->3d right-root)))
  (setq ang2 (angle '(0.0 0.0 0.0) (sg--point->3d root-start)))
  ;; Root arc should start at or ahead of the right flank root (no backward jump > 1e-3 rad).
  (setq delta (- ang2 ang1))
  (if (>= delta -1e-3)
      'pass
      (list 'fail 'root-arc-backward delta)))

(princ)
