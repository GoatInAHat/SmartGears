# SmartGears

SmartGears is a modular AutoLISP toolkit for generating 2D external spur gear profiles compatible with AutoCAD Desktop and AutoCAD Web.

## Getting started
1. Run the bundler to concatenate modules into a single distributable file:
   ```bash
   ./bundle.sh 
   ```
2. Load `SmartGears.lsp` in AutoCAD (Desktop or Web) using **APPLOAD** (desktop) or the **LOAD** command (Web/desktop), then run the command:
   ```
   SGMVP
   ```
   For general AutoLISP loading guidance, see the AutoCAD help on running LISP programs (e.g., AppLoad or LOAD). [lee-mac.com](https://www.lee-mac.com/runlisp.html?utm_source=chatgpt.com)

## Usage
`SGMVP` is the zero-input smoke test command:
- Draws a 10‑tooth external spur gear at the origin (module 2 mm, PA 20°, bore 5 mm).
- Uses layer `SGEARS` when available; otherwise falls back to the current layer.
- Attaches gear metadata (teeth/module/PA/version) via XData for later phases.
- Intended for quick verification that bundling + geometry generation are working in AutoCAD Web/Desktop.

## Repository layout
- `src/commands/` — one file per command (currently `SGMVP.lsp`)
- `src/` — modular AutoLISP source files (math/geom/topology/acad layers)
- `tests/` — smoke tests and math/geometry checks
- `.github/workflows/` — CI workflows
- `bundle.sh` — bundles modules into `SmartGears.lsp`

## Web compatibility
- Only `.lsp` files are produced for AutoCAD Web compatibility; compiled formats (FAS/VLX) are intentionally omitted.
- SmartGears avoids ActiveX/VLAX/VLA calls and desktop-only dialogs; commands use standard AutoLISP and command-line inputs to remain portable across Desktop and Web.

## Continuous integration and releases
- GitHub Actions bundles `SmartGears.lsp`, runs smoke + geometry tests, and uploads the artifact on every push/dispatch.
- When changes land on `main`, the workflow also creates a **draft release** (tagged `draft-<run_number>`) with `SmartGears.lsp` attached so it can be reviewed and published manually.

## Notes
- V1 supports external spur gears only. Future releases may add internal gears, fillets, or parameterized tolerancing.
