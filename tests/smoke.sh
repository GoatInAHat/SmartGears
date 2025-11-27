#!/usr/bin/env bash
# Minimal smoke test placeholder for SmartGears.
# In future, hook into an AutoLISP interpreter or AutoCAD test harness.

set -euo pipefail

if [[ ! -f "SmartGears.lsp" ]]; then
  echo "SmartGears.lsp not found. Run ./bundle.sh first." >&2
  exit 1
fi

if ! grep -q "(defun SGEARMAKE (params" SmartGears.lsp; then
  echo "SGEARMAKE signature not found in bundle." >&2
  exit 1
fi

cat <<'EOF'
Smoke test placeholder: SmartGears.lsp exists and is ready for load testing.
Non-interactive invocation example (run inside AutoCAD or compatible runtime):
  (SGEARMAKE '(:module 2 :teeth 12 :pressure-angle 20 :bore 5 :kerf 0.0))
This CI step only validates the bundle structure because no AutoLISP engine is available here; see TESTING.md for manual steps
to exercise the non-interactive flow.
EOF
