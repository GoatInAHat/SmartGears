#!/usr/bin/env bash
# Auto-load smoke + python test harness: bundle sources, check structure, run unit tests.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

chmod +x bundle.sh
./bundle.sh

BUNDLE="SmartGears.lsp"
if [[ ! -f "$BUNDLE" ]]; then
  echo "Bundle missing after build." >&2
  exit 1
fi

python - <<'PY'
import pathlib, sys
bundle = pathlib.Path("SmartGears.lsp")
text = bundle.read_text(encoding="utf-8")
stack = 0
for idx, ch in enumerate(text, start=1):
    if ch == '(':
        stack += 1
    elif ch == ')':
        stack -= 1
    if stack < 0:
        sys.stderr.write(f"Unbalanced parentheses near position {idx}\n")
        sys.exit(1)
if stack != 0:
    sys.stderr.write("Unbalanced parentheses in SmartGears.lsp\n")
    sys.exit(1)
required = ["c:SGEARMAKE", "sg:gear-make", "sg:acad-draw-gear", "sg:gear-derived-values", "sg:val-or-default", "(defun VAL"]
missing = [name for name in required if name not in text]
if missing:
    sys.stderr.write(f"Missing expected symbols: {', '.join(missing)}\n")
    sys.exit(1)
print("Bundle syntax check passed; core symbols present.")
PY

# Run Python-based verification of math expectations
python -m unittest discover -v tests

echo "Auto-load smoke + math tests completed successfully."
