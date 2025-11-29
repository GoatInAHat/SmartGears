# AGENTS — Operating Rules for SmartGears (AutoLISP, AutoCAD Web)

## 1) Read First, Then Act
- Before changing code, load and skim these references:
  - `SMARTGEARS_SPEC.md` (architecture, data models, commands).
  - `ROADMAP.md` (phase goals & gating criteria).
  - Offline AutoLISP docs in `docs-autolisp/` relevant to any function you call (e.g., entmake, command, ssget, polar, angle, distance). Use them at the start of a task.
  - Research papers in `research/` when touching geometry, non-circular gears, or optimization.

## 2) Platform Constraints
- Target AutoCAD Web + Desktop using core AutoLISP only; **no ActiveX, no DCL/GUI** beyond command-line prompts.
- Geometry is 2D only; outputs are closed polylines suitable for laser/DXF export.
- Math/geometry modules must stay CAD-free; AutoCAD calls live only in `acad-*` adapters.

## 3) Bundling & Modularity
- Keep code modular; GitHub Action concatenates modules into `SmartGears.lsp`. Update `bundle/manifest` if you add modules. Do not break or bypass the bundling workflow.
- Never delete or reformat anything inside `docs-autolisp/`; regenerate only via `scripts/build_offline_autolisp_docs.py` when explicitly needed.

## 4) Test-Driven-Development: Tests Are Mandatory
- All existing tests must pass; add tests for new behavior. Do not claim readiness until tests succeed and test coverage should be 100% (excluding code that is impossible to test without running AutoCAD itself). These tests should be high quality, precise, and should test things that actually matter (checking real generated geometry, not just writing bullshit tests for the sake of reaching 100% coverage).
- Follow spec’s purity boundary: unit-test math/geom without CAD side effects. Run smoke checks for bundle generation when feasible.
- For bug fixes or new features, first write failing tests that will only pass when the final, end result behavior is fixed/complete and iteratively run the tests, making changes until they pass without human intervention. If 3 cycles of this fail, question the tests and restart or ask for human intervention.
- Run `./tests/run.sh` before calling a task done; if you touch the manifest or command registration, also run `./tests/auto_load_test.sh` (bundles + structure + unit suite).

## 5) Human Input Required for Real-World Parameters
- Stop and ask when unsure about kerf, material thickness, tolerance, backlash, or other fabrication parameters. Do **not** guess default kerf/backlash values.

## 6) Tolerances & Materials
- Respect the tolerance model (kerf, backlash, bore oversize). Only use user-confirmed presets. Flag missing values.

## 7) Command & UX Guardrails
- Preserve the zero-input MVP command `SGMVP`. Do not remove or change its defaults.
- Keep prompts concise; prefer `getint/getreal/getkword`; provide sensible defaults from spec/roadmap.

## 8) Documentation Discipline
- Cite which offline pages you used in summaries (use `_navmap` paths). Reference research papers with page/section when relevant to design choices.

## 9) Communication & Safety
- Be concise; avoid needless verbosity. Call out risks and assumptions.
- Do not perform destructive git actions (`reset --hard`, etc.). If unexpected user changes appear, pause and ask.

## 10) Status Reporting
- Final messages must include a readiness statement (e.g., “Ready to submit PR!” or blockers listed). If tests aren’t run or fail, say so.

## 11) LLM Management
- Agents like you surpass humans at tasks for which there are millions of examples, but fumble through codebases in niche languages and libraries like this one and hallucination rates tend to skyrocket. Information on AutoLISP is not built into your model's weights (in other words, your "muscle memory") like it is for web development. This means the information has to be kept in the context window at all times. This is why whenever you begin a new task you must thoroughly read through the relevant AutoLISP docs (available offline in `docs-autolisp/`) and, especially if the task at all involves gear generation math/algorithms, geometry, non-circular gears, or optimization, the 7 research papers available in `research/`.

## 12) Git Hygiene
- After completing each prompt task, `git stash push -m "<task summary>"` your changes so they can be reapplied or dropped easily later. Do not stash or discard user-created work you didn't touch.
