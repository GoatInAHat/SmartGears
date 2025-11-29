#!/usr/bin/env bash
# Minimal smoke test placeholder for SmartGears.
# In future, hook into an AutoLISP interpreter or AutoCAD test harness.

set -euo pipefail

./tests/auto_load_test.sh

cat <<'EOF'
Smoke test placeholder: SmartGears.lsp bundled with scaffolded modules.
Load the bundle in AutoCAD Web/Desktop and run SGMVP to confirm the stub command loads.
This CI step only validates bundle syntax because no AutoLISP engine is available here.
EOF
