;;; bundle/manifest.lsp -- Ordered module list for SmartGears bundling.
;;; Purpose: drive bundle.sh and allow dev loading in correct dependency order.

(setq *sg-module-order*
  '(
    "src/math/math-util.lsp"
    "src/math/math-angle.lsp"
    "src/math/math-vector.lsp"
    "src/core/gear-params.lsp"
    "src/geom/geom-involute.lsp"
    "src/geom/geom-spur.lsp"
    "src/geom/geom-offset.lsp"
    "src/topology/topology-graph.lsp"
    "src/topology/topology-kinematics.lsp"
    "src/acad/acad-entities.lsp"
    "src/acad/acad-layer.lsp"
    "src/acad/acad-metadata.lsp"
    "src/acad/acad-snap.lsp"
    "src/commands/SGMVP.lsp"
    "src/commands/SGSPUR.lsp"
  ))

(defun sg-load-modules ( / )
  "Development helper: load all modules in manifest order."
  (mapcar 'load *sg-module-order*))

(princ)
