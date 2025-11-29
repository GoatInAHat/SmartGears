;;; test_sgspur_command.lsp -- Regression test for SGSPUR mock run without prompts.

(defun sg--load-bundle ( / path)
  (cond
    ((findfile "SmartGears.lsp") (setq path "SmartGears.lsp"))
    ((findfile "../SmartGears.lsp") (setq path "../SmartGears.lsp")) )
  (if path (load path)))

(defun sg-test-sgspur-mock-run ( / params result pts cr meta)
  (sg--load-bundle)
  ;; Supply parameters directly to avoid interactive prompts.
  (setq params (list (cons 'teeth 12)
                     (cons 'module 5.0)
                     (cons 'pa 20.0)
                     (cons 'bore 12.0)
                     (cons 'tol "NONE")))
  (setq result (sgspur-run T params))
  (setq pts (cdr (assoc 'points result)))
  (setq cr (cdr (assoc 'contact-ratio result)))
  (setq meta (cdr (assoc 'metadata result)))
  (cond
    ((null result) (list 'fail 'nil-result))
    ((null pts) (list 'fail 'missing-points))
    ((not (equal (car pts) (car (last pts)) 1e-6)) (list 'fail 'open-loop))
    ((or (null cr) (< cr 1.2)) (list 'fail 'contact-ratio cr))
    ((not (equal (cdr (assoc 'tol-preset meta)) "NONE")) (list 'fail 'tol-meta meta))
    (T 'pass)))

(princ)
