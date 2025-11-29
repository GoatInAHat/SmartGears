#!/usr/bin/env bash
# Auto-load smoke test: bundle sources, confirm structure, ensure SGMVP stub exists.
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

python3 - <<'PY'
import pathlib, sys
bundle = pathlib.Path("SmartGears.lsp")
text = bundle.read_text(encoding="utf-8")

stack = 0
in_string = False
in_comment = False
escaped = False
for idx, ch in enumerate(text, start=1):
    if in_comment:
        if ch == '\n':
            in_comment = False
        continue
    if escaped:
        escaped = False
        continue
    if ch == '\\':
        escaped = True
        continue
    if ch == '"':
        in_string = not in_string
        continue
    if not in_string and ch == ';':
        in_comment = True
        continue
    if in_string:
        continue
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
required = ["c:SGMVP", "*sg-module-order*"]
missing = [name for name in required if name not in text]
if missing:
    sys.stderr.write(f"Missing expected symbols: {', '.join(missing)}\n")
    sys.exit(1)
print("Bundle syntax check passed; manifest and SGMVP stub present.")
PY

# Run unit tests with the lightweight interpreter
python3 tests/run.py

echo "Auto-load smoke test completed successfully."
