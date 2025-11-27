# SmartGears Compatibility Report

## Scope
SmartGears targets AutoCAD Desktop and AutoCAD Web using plain `.lsp` source. This report captures supported features and what must be avoided so modules remain portable across both environments.

## Supported usage patterns
- Use core AutoLISP list/number utilities and entity creation commands (e.g., `ENTMAKE`, `COMMAND` with basic 2D entities such as lines, circles, polylines). These are standard features available in AutoCAD and do not rely on external automation layers.
- Keep gear logic in pure functions (no side effects) to stay portable and testable, then call drawing helpers for entity creation.
- Prefer command-line prompts and text feedback instead of custom dialogs or forms to align with Web capabilities.

## Restrictions for AutoCAD Web compatibility
- **Avoid ActiveX/VLAX/VLA APIs.** AutoCAD Web does not support ActiveX automation; routines using these functions will fail.\
  Source: Autodesk “About Developing Applications With ActiveX Automation” (ActiveX not supported in Web). [link](https://help.autodesk.com/view/ACDLT/2025/ENU/?guid=GUID-2090E4E8-9AE0-4E01-B5EB-0843A30EB0E9&utm_source=chatgpt.com)
- **Do not depend on VLX/FAS or compiled binaries.** Web supports only loading `.lsp` files directly.
- **Avoid file dialogs and desktop-only UI.** Use command-line input instead to ensure Web users can run commands.
- **Limit to 2D entity creation.** AutoCAD LT/Web notes that 3D object creation and some advanced entity types may be unsupported; keep SmartGears output to 2D gear profiles.\
  Source: Autodesk function support notes for LT/Web highlighting limited API surface. [link](https://help.autodesk.com/view/ACDLT/2025/ENU/?guid=GUID-037BF4D4-755E-4A5C-8136-80E85CCEDF3E&utm_source=chatgpt.com)

## Coding patterns to maximize portability
- Keep modules separable: math (`sg-math.lsp`) with no AutoCAD calls; data modeling in `sg-model.lsp`; drawing helpers in `sg-acad.lsp`; loader/commands in `sg-load.lsp`.
- Provide simple command entry points (prefixed `SGEAR…`) that rely on core commands and entity creation, avoiding automation libraries.
- When adding new dependencies, confirm they are part of the standard AutoLISP/command set and not tied to ActiveX or platform-specific features before inclusion.
