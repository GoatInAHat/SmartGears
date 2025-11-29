;;; test_bundle_load.lsp -- Smoke test stub to verify manifest and placeholder wiring.
;;; Intended to be loaded after SmartGears.lsp; checks placeholder and module list.

(defun sg-test-bundle-ready ( / manifest-ok placeholder-ok)
  (setq manifest-ok (and (boundp '*sg-module-order*) (listp *sg-module-order*)))
  (setq placeholder-ok (eq *sg-placeholder* :sg-placeholder))
  (if (and manifest-ok placeholder-ok)
      'ready
      (list 'not-ready manifest-ok placeholder-ok)))

(princ)
