;;; test_mirror_flank.lsp -- Ensure flank mirroring flips X (Y-axis mirror) for spur teeth.

(defun sg-test-mirror-flank ( / pt mirrored)
  (setq pt '(1.0 2.0))
  (setq mirrored (sg--mirror-x pt))
  (if (and (equal (car mirrored) 1.0 1e-6)
           (equal (cadr mirrored) -2.0 1e-6))
      'pass
      (list 'fail 'mirror-wrong mirrored)))

(princ)
