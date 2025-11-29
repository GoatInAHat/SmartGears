;;; SGSPUR.lsp -- Parameterized external spur gear command (Phase 1).

(setq *sgspur-mock* nil) ;; test hook: when non-nil, returns geom/meta instead of drawing.

;; -----------------------
;; Prompt helpers
;; -----------------------
(defun sgspur--fmt-real (val / )
  (if (numberp val) (rtos val 2 3) ""))

(defun sgspur--value-or-default (val default)
  (if (numberp val) val default))

(defun sgspur--prompt-int (label default minval / prompt val)
  (setq prompt (if default
                   (strcat "\n" label " [" (itoa (fix default)) "]: ")
                   (strcat "\n" label ": ")))
  (setq val (sgspur--value-or-default (getint prompt) default))
  (while (or (null val) (and minval (< val minval)))
    (if (null val) (princ "\nA value is required."))
    (if (and minval (< val minval))
        (princ (strcat "\nMinimum allowed is " (itoa (fix minval)) ".")))
    (setq val (sgspur--value-or-default (getint prompt) default)))
  val)

(defun sgspur--prompt-real (label default minval / prompt val)
  (setq prompt (if default
                   (strcat "\n" label " [" (sgspur--fmt-real default) "]: ")
                   (strcat "\n" label ": ")))
  (setq val (sgspur--value-or-default (getreal prompt) default))
  (while (or (null val) (and minval (< val minval)))
    (if (null val) (princ "\nA value is required."))
    (if (and minval (< val minval))
        (princ (strcat "\nMinimum allowed is " (sgspur--fmt-real minval) ".")))
    (setq val (sgspur--value-or-default (getreal prompt) default)))
  val)

(defun sgspur--join-options (opts / out)
  (setq out "")
  (foreach o opts
    (setq out (if (= out "") o (strcat out "/" o))))
  out)

(defun sgspur--prompt-preset ( / opts default-name prompt choice preset)
  (setq opts (mapcar 'car *sg-tolerance-presets*))
  (setq default-name (cdr (assoc 'name (sg-active-tolerance))))
  (if (null default-name) (setq default-name "NONE"))
  (setq prompt (strcat "\nMaterial/tolerance preset ["
                       (sgspur--join-options opts)
                       "] <" default-name ">: "))
  (setq choice (getkword prompt))
  (if (null choice) (setq choice default-name))
  (setq preset (sg-get-tolerance-preset choice))
  (if preset preset (sg-get-tolerance-preset "NONE")))

(defun sgspur--preset-from-params (params / name preset)
  (setq name (or (cdr (assoc 'tol params))
                 (cdr (assoc 'tol-preset params))
                 (cdr (assoc 'tol_name params))))
  (setq preset (if name (sg-get-tolerance-preset name) nil))
  (if preset preset (sg-active-tolerance)))

(defun sgspur--build-metadata (gear geom tol version / meta)
  (setq meta
    (list
      (cons 'id (cdr (assoc 'id gear)))
      (cons 'type "spur-ext")
      (cons 'tol-preset (cdr (assoc 'name tol)))
      (cons 'teeth (cdr (assoc 'teeth gear)))
      (cons 'module (cdr (assoc 'module gear)))
      (cons 'pa (cdr (assoc 'pa gear)))
      (cons 'bore (cdr (assoc 'bore gear)))
      (cons 'kerf (cdr (assoc 'kerf gear)))
      (cons 'backlash (cdr (assoc 'backlash gear)))
      (cons 'bore-oversize (cdr (assoc 'bore-oversize gear)))
      (cons 'pitch-diam (or (cdr (assoc 'pitch-diameter geom)) (* 2 (cdr (assoc 'pitch-radius geom)))))
      (cons 'base-diam (or (cdr (assoc 'base-diameter geom)) (* 2 (cdr (assoc 'base-radius geom)))))
      (cons 'pitch-radius (cdr (assoc 'pitch-radius geom)))
      (cons 'base-radius (cdr (assoc 'base-radius geom)))
      (cons 'version version)))
  meta)

;; -----------------------
;; Core runner
;; -----------------------
(defun sgspur-run (mock params / layer noninteractive teeth module pa bore tol kerf backlash bore-ovr gear geom pts rp cr meta layer-ok poly)
  (setq layer "SGEARS")
  (setq noninteractive (and params (listp params)))

  ;; Parameter prompts with cascading defaults; params skip prompts for tests.
  (setq teeth (if noninteractive
                  (sgspur--value-or-default (cdr (assoc 'teeth params)) 20)
                  (sgspur--prompt-int "Teeth" nil 6)))
  (setq module (if noninteractive
                   (sgspur--value-or-default (cdr (assoc 'module params)) (sg-spur-default-module teeth))
                   (sgspur--prompt-real "Module" (sg-spur-default-module teeth) 0.1)))
  (setq pa (if noninteractive
               (sgspur--value-or-default (cdr (assoc 'pa params)) 20.0)
               (sgspur--prompt-real "Pressure Angle" 20.0 1.0)))
  (setq bore (if noninteractive
                 (sgspur--value-or-default (cdr (assoc 'bore params)) (sg-spur-default-bore module teeth))
                 (sgspur--prompt-real "Bore Diameter" (sg-spur-default-bore module teeth) 0.1)))
  (setq tol (if noninteractive (sgspur--preset-from-params params) (sgspur--prompt-preset)))

  (setq kerf (sg-ensure-number (cdr (assoc 'kerf tol)) 0.0))
  (setq backlash (sg-ensure-number (cdr (assoc 'backlash tol)) 0.0))
  (setq bore-ovr (sg-ensure-number (cdr (assoc 'bore_oversize tol)) 0.0))

  (setq gear (list
               (cons 'id (sg-next-gear-id "SG"))
               (cons 'type "spur-ext")
               (cons 'teeth teeth)
               (cons 'module module)
               (cons 'pa pa)
               (cons 'bore bore)
               (cons 'kerf kerf)
               (cons 'backlash backlash)
               (cons 'bore-oversize bore-ovr)
               (cons 'tol-preset (cdr (assoc 'name tol)))
               (cons 'version "0.2")))

  (setq geom (sg-generate-spur-gear-geom gear))
  (setq pts (cdr (assoc 'points geom)))
  (setq rp (cdr (assoc 'pitch-radius geom)))
  (setq cr (sg-spur-contact-ratio gear gear))

  ;; Apply kerf/backlash thinning only when offsets are non-zero
  (setq pts (sg-apply-kerf-backlash pts kerf backlash rp))

  (setq meta (sgspur--build-metadata gear geom tol "0.2"))

  (if mock
      (list (cons 'geom geom) (cons 'metadata meta) (cons 'points pts) (cons 'contact-ratio cr))
      (progn
        (setq layer-ok (sg-ensure-layer layer))
        (if (not layer-ok)
            (princ "\nSGSPUR warning: could not create SGEARS layer; using current layer."))

        (sg-entmake-polyline pts (if layer-ok layer nil))
        (setq poly (entlast))
        (sg-write-gear-xdata poly meta)

        (if (and cr (< cr 1.4))
            (princ "\nSGSPUR warning: contact ratio below 1.4; consider more teeth."))

        (princ "\nSGSPUR: created external spur gear.")
        (princ))))

(defun c:SGSPUR ( / )
  "Parameterized external spur gear with computed defaults."
  (sgspur-run *sgspur-mock* nil))

(princ)
