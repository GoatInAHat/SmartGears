# Testing SmartGears (for AI agents)

## Non-negotiable workflow per task
- Run `./tests/run.sh` before claiming any work is complete; it bundles then executes all unit tests.
- When adding or changing behavior, first add a failing test in `tests/unit/` that captures the expected outcome, then implement the fix/feature, and rerun `./tests/run.sh` until green.
- Keep new tests CAD-free: exercise math/geom functions directly; stub/mock AutoCAD calls through existing helpers (`*sg-mock*`, `sg-mvp-run` with `T`) instead of `command`/`entmake`.
- If you touch bundling order or command registration, also run `./tests/auto_load_test.sh` (includes parentheses check + unit suite).

## Available scripts
- `./tests/run.sh` — single entry point: bundles via `bundle.sh`, then runs all unit tests under `tests/unit/` with the lightweight interpreter.
- `./tests/auto_load_test.sh` — bundle, structural sanity checks, and full unit suite; use when manifest/command files move.
- `./tests/smoke.sh` — CI-friendly wrapper that calls `auto_load_test.sh` and emits notes (no manual steps).

## MVP command reference for tests
- `SGMVP` draws a 10‑tooth external spur gear at the origin (module 2 mm, PA 20°, bore 5 mm) on layer `SGEARS` if available; metadata is written via XData.
- For geometry-only assertions, set `*sg-mock*` to `T` and call `(sg-mvp-run T)` to obtain the geometry/metadata alist without CAD side effects.
