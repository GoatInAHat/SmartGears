# Roadmap — SmartGears (AutoLISP, AutoCAD Web)

> Priority-ordered phases that preserve modularity, CAD/Web compatibility, and testability. Each phase must keep the bundling pipeline intact and retain the zero-input MVP command.

## Phase 0 — Zero-Input MVP (shipping gate)
- Command `SGMVP`: create a 10‑tooth external spur gear at the origin with module 2 mm, PA 20°, bore 5 mm, no prompts.
- Draw pitch and base circles for inspection; store metadata (teeth, module, PA, bore, version).
- Bundle into `SmartGears.lsp`; add unit tests for involute generation and polyline closure.

## Phase 1 — Parameterized Spurs (ext + int)
- Command `SGEARMAKE` with prompts for teeth, module/DP, PA, bore, type (external/internal), debug toggle.
- Geometry in pure `geom-*`; CAD output via `acad-entities` using `entmake/entmakex`.
- Validation: contact ratio ≥1.4; warn on undercut; store metadata.

## Phase 2 — Snapping & Metadata Graph
- Command `SGEARSNAP`: pick two gears; compute center distance + phase align; optional clearance override.
- Standardize XData schema for gear metadata; helper to read/write metadata.
- Smoke tests: snap 20T/40T pair; verify center distance = m*(T1+T2)/2.

## Phase 3 — Tolerances & Material Presets
- Add material presets file (kerf, backlash, bore oversize) with user-confirmed values.
- Apply kerf/backlash to tooth thickness and center distance; support per-gear overrides.
- UX: `SGEARMAT` to pick preset and apply to selected gears.

## Phase 4 — Gear Systems & Animation
- Data model for GearSystem (graph + axles); command `SGEARSET` to build/save a system from selected gears.
- Command `SGEARANIM`: propagate RPM/direction through graph; rotate entities for N frames; ESC to stop.
- Tests: 3-gear train ratio propagation; animation completes without entity loss.

## Phase 5 — Non-Circular & Custom Profiles
- Command `SGEARNC` supporting: ellipse template, Bernoulli lemniscate, CSV import of polar r(θ).
- Implement transmission function + mating gear synthesis (Xu 2020); allow torque bounding option.
- Validation: enforce geometric constraints; flag collisions.

## Phase 6 — Compound/Planetary Kits
- Support compound gears on shared axle; ring/internal meshes.
- Command `SGEARPLANET`: sun/planet/ring placement with Tr = Ts + 2*Tp; optional carrier outline.
- Extend animation to planetaries.

## Phase 7 — Optimization-Assisted Layout (optional)
- Expose DE/NSGA-II hooks to search gear count/module/center distances for objectives (mass/area/efficiency) under constraints.
- Allow dry-run mode that emits candidate parameters without drawing.

## Always-on Tasks
- Keep math/geom AutoCAD-free; all CAD calls isolated.
- Maintain bundling manifest order; every new module included in `SmartGears.lsp` build.
- Update tests and docs each phase; do not regress `SGMVP` behavior.

