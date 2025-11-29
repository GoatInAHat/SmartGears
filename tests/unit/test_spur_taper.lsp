;;; test_spur_taper.lsp -- Ensure spur teeth taper inward toward the tip (SGMVP default).

(defun sg--angle0 (pt)
  (angle '(0.0 0.0) pt))

(defun sg-test-spur-taper ( / geom pts left-base right-base left-tip right-tip
                                 sep-base sep-tip)
  ;; Use the zero-input SGMVP gear (10T, m=2) as a fixture.
  (setq geom (sg-generate-spur-gear-geom (sg-mvp-params)))
  (setq pts (cdr (assoc 'points geom)))

  ;; First tooth ordering: left flank (root->tip) [0..23], right flank reversed (tip->root) [24..47].
  (setq left-base  (nth 0 pts))
  (setq left-tip   (nth 23 pts))
  (setq right-tip  (nth 24 pts))
  (setq right-base (nth 47 pts))

  (setq sep-base (abs (- (sg--angle0 right-base) (sg--angle0 left-base))))
  (setq sep-tip  (abs (- (sg--angle0 right-tip)  (sg--angle0 left-tip))))

  (if (< sep-tip sep-base)
      'pass
      (list 'fail 'tooth-not-tapering sep-base sep-tip)))

(princ)
