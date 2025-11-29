;;; acad-metadata.lsp -- Metadata helpers for SmartGears entities.
;;; Purpose: pack/unpack gear parameters into XData-safe alists.

(defun sg--ensure-regapp (name)
  (if (not (tblsearch "APPID" name))
      (regapp name)))

(defun sg-write-gear-xdata (ent metadata / app xdata entdata cleaned)
  "Attach minimal SmartGears metadata to entity ENT.
METADATA expects keys: id (string), teeth (int), module (real), pa (deg), version (string)."
  (if ent
      (progn
        (setq app "SMARTGEARS")
        (sg--ensure-regapp app)
        ;; XData must be wrapped in group -3 with app name then pairs
        (setq xdata (list
                      (list -3
                            (append
                              (list app)
                              (list
                                (cons 1000 (cdr (assoc 'id metadata)))
                                (cons 1070 (cdr (assoc 'teeth metadata)))
                                (cons 1040 (cdr (assoc 'module metadata)))
                                (cons 1040 (cdr (assoc 'pa metadata)))
                                (cons 1000 (cdr (assoc 'version metadata))))))))
        (setq entdata (entget ent '("*")))
        (if entdata
            (progn
              ;; remove existing SMARTGEARS xdata
              (setq cleaned '())
              (foreach e entdata
                (if (and (= (car e) -3) (listp (cadr e)) (equal (car (cadr e)) app))
                    nil
                    (setq cleaned (append cleaned (list e)))))
              (entmod (append cleaned xdata)))))))

;; Aliases for spec wording
(defun write-gear-xdata (ent metadata) (sg-write-gear-xdata ent metadata))

(princ)
