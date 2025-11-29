# SmartGears Technical Specification (AutoLISP, AutoCAD Web)

> Scope: 2D laser-cuttable gear generation, meshing, and lightweight motion simulation for AutoCAD Web using pure AutoLISP (no ActiveX/.NET). Geometry must remain planar; math/geometry layers stay CAD-independent. Final distribution is a single `SmartGears.lsp` produced by the bundling action.

---

## 1. Ground Rules & Constraints

- **Platform**: AutoCAD Web + Desktop; only AutoLISP core + `entmake/entmakex`, `command`, `ssget`, `polar`, `angle`, `distance` (AutoLISP Reference, Web-supported). No ActiveX/DCL; rely on command-line prompts.
- **Purity boundary**: `src/math` & `src/geom` are side-effect free (no AutoCAD calls). `src/acad` handles entity creation via `entmake/entmakex` (Docs: entmake, entmakex). Animation uses `command`/`ENTMOD` with throttled loops.
- **Geometry scope**: 2D profiles only; outputs are closed lightweight polylines suitable for DXF/DWG export and laser cutting.
- **Bundling**: Keep modules modular; GitHub Action concatenates into `SmartGears.lsp` (see AGENTS §8.1).
- **Tolerances**: Require explicit kerf/clearance inputs per material; do not hard-code unverified values (human confirmation required for kerf/backlash defaults).
- **Contact ratio**: Maintain ≥1.4 for spur meshes to avoid undercutting (Savage et al., Optimal Design of Standard Gearsets, p.10).
- **Non-circular gears**: Represent pitch as polar function r(θ); transmission constraints per Xu et al. 2020 (Computational Design and Optimization of Non-Circular Gears, pp.3‑6).

---

## 2. Architecture Overview

### 2.1 Layering
1. **math/** — scalar/vector helpers, radians-only trig, matrix transforms.
2. **geom/** — gear/tooth geometry in model space (no CAD). Spur involute, internal, nautilus, non-circular pitch sampling, fillets, offsets.
3. **topology/** — graphs of gears, axles, meshes, compound groups; validation and kinematic propagation.
4. **acad/** — CAD adapters: polyline/arc builders, block wrapper, XData storage, snap/rotate/move helpers using `entmake`, `command`, `ssget`.
5. **ui/** — command definitions (CLI-only). Defaults + validation; optional debug/dry-run modes.
6. **bundle/** — orchestrates module load order; produces single-file build for AutoCAD Web.

### 2.2 Module responsibilities
- `math-angle.lsp`: normalize angles, wrap ±π, degree↔rad conversions.
- `geom-involute.lsp`: involute point from base circle, mirrored flank builder, root fillet approximation.
- `geom-spur.lsp`: external/internal tooth loop generator, whole gear polygon generation, pitch/base/root circles.
- `geom-nc.lsp`: polar curve sampling, arc-length reparam for even tooth pitch (Xu et al. 2020, §3.3‑5), Bernoulli lemniscate helper (Zhang 2022, Eq.1‑6).
- `geom-offset.lsp`: kerf/backlash offsets (radial + angular tooth thinning).
- `topology-graph.lsp`: nodes (gear instances), edges (mesh with sign, ratio, clearance), coaxial groups, validation of loops.
- `topology-kinematics.lsp`: propagate RPM/direction through graph; compound pairs handled by shared axle constraint.
- `acad-entities.lsp`: entmake polyline, circles; layer/color; block wrapper for reuse; attach XData (gear metadata, version, tolerance preset).
- `acad-snap.lsp`: compute center distance, move/rotate second gear, phase align tooth/gap; uses `command` for MOVE/ROTATE when Web-safe.
- `ui-commands.lsp`: registers commands (MVP + later phases). Every command validates inputs, prints summary, writes metadata.
- `bundle/manifest.lsp`: ordered `(load ...)` list for GitHub Action concatenation.

---

## 3. Data Model

All data is plain association lists for Web compatibility; keys are symbols.

### 3.1 Gear (template)
- `id` (string)
- `type` (`spur-ext` | `spur-int` | `nautilus` | `nc-polar` | future)
- `teeth` (int ≥ 6 external; ≥ 12 internal to avoid undercut) 
- `module` or `dp` (exactly one stored; derived others cached)
- `pa` (pressure angle, deg; default 20)
- `pitch_diam`, `base_diam`, `addendum`, `dedendum`, `root_fillet`
- `profile_shift` (mm)
- `thickness` (metadata only)
- `material` (symbol) & `kerf`, `backlash`, `bore_oversize` (mm)
- `bore_diam`
- `rpm` (current state for simulation)
- `origin_phase` (rad; reference tooth phase)

### 3.2 GearInstance (placed gear)
- `gear` (Gear template reference)
- `center` (UCS point)
- `theta` (orientation rad)
- `block_name` / `entity_ids` (for CAD handles)
- `axle_id` (optional)

### 3.3 Axle
- `id`
- `center`
- `locked` (boolean for fixed axles)
- `gears` (GearInstance ids sharing coaxial rotation)

### 3.4 MeshEdge
- `g1`, `g2` (GearInstance ids)
- `contact` (`external` | `internal`)
- `ratio` (signed = -T1/T2 for external, + for internal) (Savage p.1)
- `center_distance_nominal`
- `clearance` (mm; from tolerance preset)
- `phase_offset` (rad) for alignment
- `status` (`ok` | `warn-clearance` | `warn-contact-ratio`)

### 3.5 GearSystem
- `gears` (map id→GearInstance)
- `axles` (map id→Axle)
- `meshes` (list of MeshEdge)
- `root_nodes` (drivers) and `ground` (fixed members)
- Derived: adjacency for traversal; cached contact ratios.

### 3.6 MaterialPreset
- `name`, `kerf`, `backlash`, `bore_oversize`, `notes`
- *Values must be confirmed by user; see §10.*

---

## 4. Geometry Generation Algorithms

### 4.1 Spur Gear (External)
- Base radius: `rb = m * T * cos(pa) / 2`.
- Pitch radius: `rp = m * T / 2`.
- Addendum: `m`; Dedendum: `1.25*m` (standard)
- Contact ratio target ≥1.4 (Savage p.10). If computed CR <1.4, warn and suggest higher teeth/module.
- Involute flank: sample angle `φ` from `φ_root` to `φ_tip`, where `φ_root` solves `distance(p, root) = dedendum`.
- Mirror flank for opposite side; rotate by `2π/T` per tooth; accumulate points; close polyline.
- Root fillet: circular arc of radius `root_fillet` blended between flank and root.

### 4.2 Spur Gear (Internal)
- Mirror involute across pitch circle, swap addendum/dedendum signs.
- Center distance: `(m/2)*(T_int - T_ext)`.
- Undercut guard: require `T_int - T_ext ≥ 6` plus CR check.

### 4.3 Nautilus / Log-Spiral (step-up) Gear
- Pitch curve: logarithmic spiral `r = r0 * e^(kθ)`; choose `k` s.t. arc length per tooth equals module pitch. Used for variable-ratio cams.
- Tooth normals follow local polar normal; thin tooth to maintain clearance.
- Tagged as experimental; generate conjugate follower using sampled transmission function.

### 4.4 Non-Circular Gears (NC)
- Represent pitch as polar samples `{θi, ri}` (Xu 2020 §3.1, N=1024 default).
- Transmission function ψ(φD) from Eq.8 (Xu 2020 p.4) using trapezoidal integration.
- Distance between centers L solved via binary search (Eq.9 Xu 2020 p.4).
- Follower pitch: `rF(π-φF) = L - rD(2π-ψ⁻¹(φF))` (Eq.10 Xu 2020 p.4).
- Teeth: place M evenly along arc length; involute flank replaced by normal offset to pitch curve at sample point (Xu 2020 §3.1 teeth model).
- Optional torque bounding by truncating ψ′ (Eq.16 Xu 2020 p.6) to meet motor torque limit.

### 4.5 Bernoulli Lemniscate Pitch (NC Special)
- Pitch curve: `r(θ) = a * sin(2θ)` over constrained interval ensuring r>0 (Zhang 2022 Eq.2‑3).
- Derivatives per Eq.4‑6 control curvature monotonicity, avoiding negative radius.
- Conjugate follower computed via same ψ integration as generic NC.

### 4.6 Tooth Thickness, Backlash, Kerf
- Nominal tooth thickness at pitch circle: `s = π*m/2`.
- Apply radial offset Δr = kerf/2 + backlash/2; angular thinning Δθ = backlash/(rp).
- Keep root thickness positive; warn if Δθ exceeds 20% of nominal.

### 4.7 Discretization & Output
- Default flank samples: 24 per flank (external), 48 for NC; adjustable via debug flag.
- Output entity: lightweight polyline with bulge arcs for fillets when possible; otherwise dense points.
- Metadata: store source parameters, tolerance preset, CRC hash of geometry for cache reuse.

---

## 5. Meshing, Alignment, and Validation

1. **Center distance**: `C = rp1 + rp2 + clearance` (external) or `C = rp_int - rp_ext - clearance` (internal).
2. **Phase alignment**: initial phase ensures tooth-to-gap alignment; compute by matching reference tooth angles modulo `2π/T`.
3. **Contact ratio check**: for spurs, compute CR = (line of action length)/(base pitch); warn <1.4.
4. **Interference guard**: ensure addendum of smaller gear < dedendum of mate minus clearance.
5. **NC compatibility**: enforce Xu 2020 constraints Eq.4‑6 during generation; flag if violated.
6. **Graph validation**: detect contradictory meshes (same pair with different ratios); detect unsatisfied center-distance tolerance > kerf allowance.
7. **Collision**: approximate gears as bounding circles (for spurs) or convex hull (NC) to detect overlap before drawing.

---

## 6. System Topology & Kinematics

- Represent system as undirected signed graph.
- For each connected component, pick driver(s); propagate RPM using signed ratios; respect coaxial axles sharing same ω.
- Compound gears (two gears on one axle) modeled via shared axle; ratio multiplies along path.
- Planetaries (later phase): ring internal + planets external; enforce `Tr = Ts + 2*Tp` for even spacing (roadmap later).
- For loops, solve using spanning tree then verify consistency; if inconsistent, mark system invalid.

---

## 7. User Interaction Model (AutoCAD Web Compatible)

- **Commands** (CLI prompts only):
  - `SGMVP` — zero-input demo: draws 10‑tooth spur (module 2mm, PA 20°, bore 5mm) at origin on layer `SGEARS`. Produces metadata + pitch/debug circles.
  - `SGEARMAKE` — parameterized spur (ext/int); prompts for teeth, module/DP, PA, bore, material preset, tolerance.
  - `SGEARSNAP` — select two gears; auto center-distance + phase alignment; optional clearance override.
  - `SGEARSET` — build simple train/compound: prompt for existing gears + meshes; stores GearSystem object in XData.
  - `SGEARANIM` — pick driver, RPM; animates system for N steps (small loop with `command "rotate"`), with ESC to stop.
  - `SGEARNC` (later): select NC template (ellipse, lemniscate, custom CSV), teeth, transmission ratio K.
  - `SGDEBUG` — toggles draw of pitch/base circles, contact lines.
- **Prompts** follow `getint/getreal/getkword` patterns; all defaults shown in brackets; Enter = accept default.
- **Undo safety**: wrap CAD mutations inside `command "._UNDO" "GROUP" ... "END"` when supported.

---

## 8. Motion & Animation Model

- **State**: each GearInstance holds current θ, ω. Animation loop updates θ ← θ + ω*Δt.
- **Rendering**: apply ROTATE to gear entities about center by Δθ each frame; Δt default 0.05s (configurable); max frames capped to prevent runaway in Web.
- **Direction**: external meshes invert sign; internal retain sign.
- **Compound**: gears on same axle share θ, ω.
- **NC animation**: approximate instantaneous ratio from ψ′(φ) (Xu 2020 Eq.14‑15) sampled at current φ; update follower ω accordingly.
- **Performance guard**: for Web, limit total entities < 3k and frames < 300; fall back to static preview if exceeded.

---

## 9. Persistence, Metadata, and Bundling

- Every drawn gear stores XData: `(SG:version "0.1" :id ... :type ... :teeth ... :module ... :pa ... :material ... :kerf ... :backlash ... :system-id ...)`.
- System graph stored on a hidden dictionary keyed by `system-id` (for Web safety keep minimal size).
- Bundling: `bundle.sh` concatenates modules per `bundle/manifest.lsp`; final `SmartGears.lsp` includes loader that sets search path, loads dependencies, and registers commands.

---

## 10. Tolerance, Kerf, and Material Guidance

- Parameters: `kerf` (radial removal, mm), `backlash` (mm at pitch circle), `bore_oversize` (mm).
- **Reference presets (industry typical laser/CNC practice; adjust per machine):**

  | Material & thickness | Kerf (mm) | Backlash (mm) | Bore oversize (mm) | Notes |
  | --- | --- | --- | --- | --- |
  | MDF 3.0 mm | 0.15 | 0.07 | 0.15 | CO₂ laser, clean focus; allow light tooth thinning. |
  | **MDF 6.4 mm** | **0.20** | **0.10** | **0.20** | CO₂ laser on 1/4" sheet; thicker char, higher clearance. |
  | Baltic birch ply 6 mm | 0.18 | 0.10 | 0.18 | Grain variation; expect slight taper. |
  | Cast acrylic 3.0 mm | 0.12 | 0.05 | 0.10 | Low char; smooth walls. |
  | PLA FFF print (0.4 mm nozzle) | 0.00 (no kerf) | 0.08 | 0.15 | Use elephant-foot compensation in slicer if needed. |

  These numbers are starting points; measure your machine’s kerf on scrap and override in commands.
- Clearance at mesh: default `clearance = kerf + backlash/2`; user override allowed.
- Export scaling: ensure all dimensions in drawing units (mm by default); if drawing in inches, convert module↔DP accordingly.

---

## 11. Testing & Validation Strategy

- **Unit tests (LISP)**: pure functions in `math/geom` with golden results (pitch diameter, base radius, involute endpoints, contact ratio calculations).
- **Geometry regression**: compare generated point lists CRC against fixtures for key cases (10T, 20T, internal 60/20, lemniscate NC).
- **Mesh validation tests**: ensure center distance & ratio calculations match analytic results; contact ratio >=1.4 for default spurs.
- **Animation smoke test**: small system animates 20 frames without entity loss.
- **Bundle test**: run bundler then load `SmartGears.lsp` into headless AutoCAD test harness (if available) or lint for missing symbols.
- **Manual checklist**: verify DXF export opens in LibreCAD and shows closed polylines.

---

## 12. Extensibility Hooks

- New gear types add modules under `geom-*` and register factory in `gear-type->generator` table.
- Optimization layer (future) can call DE/NSGA solvers (Méndez 2020 DE‑NSGA-II p.3; Dinh 2025 NSGA-II+EAMR p.1‑2; Vu 2025 NSGA-II mass/efficiency p.1‑2). Input: parameter bounds, objectives (mass, efficiency, volume), constraints (contact ratio, strength) derived from Savage p.10.
- Material database extendable via JSON-like alist file loaded at runtime.

---

## 13. Out-of-Scope / Deferred

- 3D solids, swept gears, helical gears (need 3D; not Web-safe).
- Heavy GUI/DCL; Web does not support.
- Full FEA/strength verification; only basic contact-ratio and geometry checks included.
- Real-time physics or collision during animation; only kinematic updates implemented.

---

## 14. Design Rationales (traceability)

- **Non-circular modeling** follows polar discretization + transmission integration from Xu 2020 (pp.3‑6) to guarantee mating compatibility and manageable torque bounds.
- **Pitch-curve choice** for NC includes Bernoulli lemniscate (Zhang 2022 pp.1‑2) because closed-form r(θ) and derivatives simplify sampling.
- **Contact ratio target** (≥1.4) taken from standard spur practice (Savage p.10) to minimize undercut and vibration in 2D laser-cut gears.
- **Optimization hooks** lean on DE/NSGA frameworks (Méndez 2020 p.3 problem formulation; Vu 2025 p.2 mass/efficiency objectives; Dinh 2025 p.1 dual-objective area/efficiency) to enable future automated layout tuning.
- **Pure math layer** mandated to keep bundling compatibility with AutoCAD Web and enable deterministic tests.

---

## 15. Interfaces & Command Defaults

- **SGMVP** (no prompts): T=10, m=2mm, PA=20°, bore=5mm, layer `SGEARS`, color ByLayer, kerf/backlash = 0 (expects user to adjust later). Draw pitch/base circles for inspection.
- **SGEARMAKE** prompts (defaults in brackets):
  - Teeth `[20]`, Module `[2]`, PA `[20]`, Type `[External/Internal]`, Bore `[5]`, Material `[None]`, Kerf `[ask]`, Backlash `[ask]`, Profile shift `[0]`, Draw debug `[No]`.
- **SGEARSNAP**: select two gears; option `Clearance <default from kerf/backlash>`; option `Align tooth/gap [Yes]`.
- **SGEARSET**: build mesh graph; optionally save as named system.
- **SGEARANIM**: driver selection, RPM `[60]`, steps `[120]`, step time `[0.05]`, stop on ESC.

---

## 16. Minimal Viable Product (MVP)

- Deliver `SGMVP` command plus core geometry/math modules and bundling. No user input; draws 10T spur at origin. Establishes metadata, debug circles, and test harness.
- Tests: involute point, pitch diameter, generated polyline closed; bundle produces single file.

---

## 17. Future Phases (see ROADMAP.md for ordering)

- Parameterized spur, snapping, tolerances, gear graphs, animation, NC support, optimization-driven layout, planetary kits, and export presets.
