# SmartGears

SmartGears is a modular AutoLISP toolkit for generating 2D external spur gear profiles compatible with AutoCAD Desktop and AutoCAD Web.

## Getting started
1. Run the bundler to concatenate modules into a single distributable file:
   ```bash
   ./bundle.sh
   ```
2. Load `SmartGears.lsp` in AutoCAD (Desktop or Web) using **APPLOAD** (desktop) or the **LOAD** command (Web/desktop), then run the command:
   ```
   SGEARMAKE
   ```
   For general AutoLISP loading guidance, see the AutoCAD help on running LISP programs (e.g., AppLoad or LOAD). [lee-mac.com](https://www.lee-mac.com/runlisp.html?utm_source=chatgpt.com)

## Usage
`SGEARMAKE` prompts for:
- Number of teeth (integer > 4)
- Module (mm)
- Pressure angle (degrees; default 20°)
- Bore diameter (mm)
- Kerf / clearance offset (mm; positive to expand, negative to shrink)
- Center point (defaults to origin if Enter is pressed)

The tool builds an involute spur gear outline (outer profile, root circle, bore) using only core AutoLISP primitives for maximum AutoCAD Web compatibility.

## Repository layout
- `src/` — modular AutoLISP source files
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
