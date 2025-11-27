#!/usr/bin/env bash
set -euo pipefail

OUTPUT="SmartGears.lsp"
MODULES=(
  "src/sg-math.lsp"
  "src/sg-model.lsp"
  "src/sg-acad.lsp"
  "src/sg-load.lsp"
)

rm -f "$OUTPUT"

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
