# Testing SmartGears

## Interactive vs non-interactive SGEARMAKE
- **Interactive**: run `SGEARMAKE` directly. Each prompt shows a default; pressing Enter accepts it via `sg:val-or-default`.
- **Non-interactive**: call `SGEARMAKE` with a property list (keywords lowercase), for example:
  ```lisp
  (SGEARMAKE '(:module 2 :teeth 12 :pressure-angle 20 :bore 5 :kerf 0.0))
  ```
  The call bypasses prompts and uses the provided values, defaulting any missing entry through `sg:val-or-default`.

## Automated checks
- `./bundle.sh` — concatenates modules into `SmartGears.lsp`.
- `./tests/auto_load_test.sh` — runs the bundler, checks for balanced parentheses and expected symbols, then runs Python unit tests.
- `./tests/smoke.sh` — verifies the bundled file exists and records a non-interactive `SGEARMAKE` example. CI cannot execute AutoLISP here, so this step documents the call rather than running it.

## Manual AutoLISP checks
1. Load `SmartGears.lsp` into AutoCAD (Desktop or Web) and run:
   ```
   (SGEARMAKE '(:module 2 :teeth 12 :pressure-angle 20 :bore 5 :kerf 0.0))
   ```
   to confirm the non-interactive mode draws without prompting.
2. Load `tests/sg-input-tests.lsp` after `SmartGears.lsp` and evaluate `(sg:run-input-tests)` to verify `sg:val-or-default` fallback behavior.
3. Optionally run `SGEARMAKE` without parameters to confirm the prompts accept Enter for defaults and still draw the gear.
