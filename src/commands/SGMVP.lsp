;;; SGMVP.lsp -- Zero-input MVP command in its own module.
;;; Purpose: define the SGMVP command and helpers without bundling other commands.

(setq *sg-mock* nil) ;; test hook: when non-nil, SGMVP returns geometry only.

(defun sg--ensure-layer (name / existing created)
  "Ensure LAYER table contains NAME. Returns T if available."
  (setq existing (tblsearch "LAYER" name))
  (cond
    (existing T)
    ;; Attempt lightweight creation first; entmake returns nil on invalid DXF.
    ((setq created (entmake (list '(0 . "LAYER")
                                  (cons 2 name)
                                  '(70 . 0)
                                  '(62 . 7)
                                  '(6 . "CONTINUOUS"))))
     T)
    ;; Fallback to command call without relying on ERRSET (not present on Web).
    ((progn
       (command "._-LAYER" "_New" name "")
       (tblsearch "LAYER" name)))
    (T nil)))

(defun sg-mvp-params ( / )
  "Return alist of default MVP gear parameters."
  (list (cons 'id "SGMVP-DEFAULT")
        (cons 'teeth 10)
        (cons 'module 2.0)
        (cons 'pa 20.0)
        (cons 'bore 5.0)
        (cons 'kerf 0.0)
        (cons 'backlash 0.0)
        (cons 'layer "SGEARS")
        (cons 'version "0.1")))

(defun sg-mvp-run (mock / gear geom points rp rb layer target-layer layer-ok poly-ent meta)
  "Core implementation for SGMVP. When MOCK is non-nil, returns geometry only."
  (setq gear (sg-mvp-params))
  (setq geom (sg-generate-spur-gear-geom gear))
  (setq points (cdr (assoc 'points geom)))
  (setq rp (cdr (assoc 'pitch-radius geom)))
  (setq rb (cdr (assoc 'base-radius geom)))
  (setq layer (cdr (assoc 'layer gear)))
  (setq meta (list (cons 'id (cdr (assoc 'id gear)))
                   (cons 'teeth (cdr (assoc 'teeth gear)))
                   (cons 'module (cdr (assoc 'module gear)))
                   (cons 'pa (cdr (assoc 'pa gear)))
                   (cons 'version (cdr (assoc 'version gear)))))

  (if mock
      (list (cons 'geom geom) (cons 'metadata meta))
      (progn
        (setq layer-ok (sg--ensure-layer layer))
        (setq target-layer (if layer-ok layer nil))
        (if (not layer-ok)
            (princ "\nSGMVP warning: could not create SGEARS layer; using current layer."))

        ;; Draw gear polyline
        (sg-entmake-polyline points target-layer)
        (setq poly-ent (entlast))

        ;; Debug circles omitted per user request; keep geometry minimal

        ;; Attach XData to gear polyline
        (sg-write-gear-xdata poly-ent meta)

        (princ "\nSGMVP: created default 10T spur gear at origin.")
        (princ)
      )))

(defun c:SGMVP ( / )
  "Zero-input MVP: draws 10T spur (m=2, PA=20, bore=5) with pitch/base circles."
  (sg-mvp-run *sg-mock*))

(princ)
