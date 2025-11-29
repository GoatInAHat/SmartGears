#!/usr/bin/env bash
# Bundle SmartGears modules in manifest order into SmartGears.lsp.
# Manifest lives at bundle/manifest.lsp with *sg-module-order* list.
set -euo pipefail

MANIFEST="bundle/manifest.lsp"
OUTPUT="SmartGears.lsp"

if [[ ! -f "$MANIFEST" ]]; then
  echo "Manifest not found at $MANIFEST" >&2
  exit 1
fi

MODULES=()
while IFS= read -r module; do
  MODULES+=("$module")
done < <(python3 - <<'PY'
import pathlib, re, sys
manifest = pathlib.Path("bundle/manifest.lsp")
text = manifest.read_text(encoding="utf-8")
match = re.search(r"\*sg-module-order\*.*?\(([^)]*)\)", text, re.S)
if not match:
    sys.stderr.write("Could not locate *sg-module-order* list in manifest.lsp\\n")
    sys.exit(1)
modules = re.findall(r"\"([^\"]+)\"", match.group(1))
if not modules:
    sys.stderr.write("No module paths found in manifest.lsp\\n")
    sys.exit(1)
for mod in modules:
    print(mod)
PY
)

rm -f "$OUTPUT"

echo ";;; --- BEGIN ${MANIFEST} ---" >> "$OUTPUT"
cat "$MANIFEST" >> "$OUTPUT"
echo -e "\n;;; --- END ${MANIFEST} ---\n" >> "$OUTPUT"

for module in "${MODULES[@]}"; do
  if [[ ! -f "$module" ]]; then
    echo "Missing module: $module" >&2
    exit 1
  fi
  echo ";;; --- BEGIN ${module} ---" >> "$OUTPUT"
  cat "$module" >> "$OUTPUT"
  echo -e "\n;;; --- END ${module} ---\n" >> "$OUTPUT"
done

echo "Bundled ${#MODULES[@]} modules into $OUTPUT"
