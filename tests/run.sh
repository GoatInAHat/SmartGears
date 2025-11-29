#!/usr/bin/env bash
# Lightweight AutoLISP unit runner using the bundled SmartGears interpreter.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

./bundle.sh
python3 tests/run.py "$@"
