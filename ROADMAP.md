# ROADMAP — SmartGears 2D Gear Generator for AutoCAD

## 0. Vision & Design Principles

Goal: Build **SmartGears**, the best 2D gear generator for AutoCAD, implemented in **AutoLISP** (and optionally VLISP/ActiveX), that can:

- Generate **standard involute spur gears** (external & internal) with robust math.
- Generate **planetary systems** (sun, planets, ring) with correct spacing.
- Generate **non-circular gears** (elliptical, nautilus, user-defined).
- **Turn any closed shape into a gear** and auto-generate a matching driving gear (approximate conjugate profile).
- **Snap gears together** with automatically aligned teeth.
- **Anchor** one gear and move another around it while maintaining proper meshing.
- **Animate** gears in AutoCAD (visualization), computing RPM and direction propagation.
- Output **laser-cut-ready geometry** with adjustable tolerances and material presets.
- Offer a **clean, discoverable command set and GUI** that both engineers and casual users can use.

Design principles:

- **Modular architecture** (clear separation between geometry, domain logic, and UI).
- **Declarative configuration** (gear objects as data structures, not loose entities).
- **Idempotent generation** (reproducible from parameters; can be regenerated/updated).
- **Strong defaults** for non-expert users with full control for power users.
- **Extensibility**: every MVP decision should make later features easier, not harder.

---

## 1. High-Level Architecture

### 1.1 Core Layers

1. **Math / Geometry Layer**
   - Pure functions for:
     - Involute spur gear geometry (external & internal).
     - Center distance calculations.
     - Planetary gearing relations.
     - Non-circular parametric curves (elliptical, logarithmic/Archimedean spirals for nautilus).
   - No AutoCAD calls in this layer; just numerical lists of points, angles, etc.

2. **Gear Model Layer**
   - Data structures representing gears and systems:
     - `Gear`: type, teeth, module/DP, pressure angle, helix (for future), thickness (for metadata), bore, tolerance, etc.
     - `GearInstance`: a `Gear` plus placement info (center point, rotation, layer, color).
     - `GearSystem`: grouping for planetary sets or arbitrary meshes (graph of meshing gears).
   - Functions:
     - Constructing `Gear` objects from user input or defaults.
     - Validating gear parameters.
     - Computing resulting kinematics (gear ratios, directions, RPMs).

3. **AutoCAD Integration Layer**
   - Functions that turn geometry (point lists, polylines, arcs) into DWG entities.
   - Utility wrappers for:
     - Creating polylines, circles, regions.
     - Applying layers, colors, linetypes.
     - Inserting blocks representing a gear.
     - Simple transforms (translate, rotate, scale) and snapping.
   - Animation primitives (transforming collections of entities over time with loops).

4. **UI Layer (CLI + DCL GUI)**
   - Set of AutoCAD commands and dialogs:
     - Command-line first for MVP.
     - DCL-based or .NET-form-based GUI in v1.2 and beyond.
   - Responsible for:
     - Prompt/validate user input.
     - Offer sensible defaults and presets (e.g., “steel, PA 20°, module 2, backlash 0.05”).
     - Show “help tips” like `pla: 20` (pressure angle shortcut) while user types.

---

## 2. Command Naming & UX

### 2.1 Core Commands (External & Internal Spurs)

- `SGEARMAKE` — Main entry point; creates one gear.
- `SGEARSNAP` — Aligns two existing gears so teeth mesh & align.
- `SGEARANIM` — Animates a gear or gear system.
- `SGEARPLANET` — Creates a planetary gear set (sun/planets/ring, placed correctly).
- `SGEARMAT` — Sets material/tolerance presets and applies them.
- `SGEARNC` — (v2) Non-circular gear creation (ellipse, nautilus, etc.).
- `SGEARSHAPEPAIR` — (v2.1) Turn any closed shape into a gear and generate a matching driver gear.

### 2.2 Design Goals

- Commands should be **self-explanatory** (`SGEARMAKE`, `SGEARPLANET`, etc.).
- Parameters should:
  - Offer **short aliases** (e.g., `pla: 20` for pressure angle).
  - Show **inline suggestions** (ex: when user starts typing `mat` show `[Steel, PLA, MDF]`).
- Allow:
  - **Expert mode**: user supplies module/DP, teeth, addendum mods, backlash, etc.
  - **Simple mode**: user indicates “small gear, MDF, 3 mm bore” and everything else is auto-chosen.

---

## 3. Version Roadmap

### 3.0 MVP (v1.0) — Standard External Spur Gears Only

**Objective:** Implement a rock-solid core for **external involute spur gears**, with clean geometry and internal data model.

#### 3.0.1 Features

- Command: `SGEARMAKE`
  - Required inputs:
    - Teeth (`T`)
    - Module (`m`) OR Diametral Pitch (`DP`) (internally use one consistently)
    - Pressure angle (`α`) – default 20°
    - Bore diameter
  - Optional:
    - Face width (metadata only for now)
    - Addendum modification / profile shift (initially default 0)
- Output:
  - 2D closed polyline representing gear outline (tooth flanks + root + tip).
  - Center circle (bore).
  - Optional construction geometry (pitch circle, base circle) on separate layer for debugging.

#### 3.0.2 Internal Structure

- `sg-math.lsp`
  - `sg-involute-point(base_radius, angle)` → point
  - `sg-tooth-profile(params)` → list of points / arcs for one tooth
  - `sg-gear-geometry(params)` → polyline/arc spec for full gear
- `sg-model.lsp`
  - `sg-make-gear(teeth, module, pa, bore, opts)` → Gear object (association list or struct)
  - `sg-gear-pitch-diameter(gear)` etc.
- `sg-acad.lsp`
  - `sg-draw-gear(gear)` → entity IDs
  - `sg-make-polyline(point-list, layer, color)` helpers

#### 3.0.3 Best Practices

- All core math functions **pure** (no global state, no AutoCAD calls).
- Internal representation of angles in radians; convert to degrees only for user prompts.
- Use **named association lists** for gears, e.g.:

  ```lisp
  (setq g (list (cons 'type 'spur)
                (cons 'teeth 24)
                (cons 'module 2.0)
                (cons 'pa 20.0)
                (cons 'bore 5.0)
                (cons 'tolerance 0.0)))
  ```

- MVP does **not** handle internal gears, snapping, animation, or tolerances beyond an optional radial offset of tooth.

---

### 3.1 v1.1 — Gear Snapping & Tooth Alignment

**Objective:** Allow users to automatically **snap gears together** so their teeth mesh and align properly.

#### 3.1.1 Features

- Command: `SGEARSNAP`
  - User selects two gear instances (by picking anywhere on the gear).
  - Tool infers:
    - Centers of both gears.
    - Teeth counts from stored metadata (e.g., XData on the entity or block).
  - Computes:
    - Correct **center distance**:
      (C = m * (T1 + T2) / 2)
    - Optional extra clearance (tolerance).
  - Repositions the second gear so:
    - Its center lies at correct distance from the first.
    - Optionally rotates it so tooth–tooth or tooth–gap alignment is visually nice.

#### 3.1.2 Implementation Notes

- When generating a gear in `SGEARMAKE`, store metadata on the gear entity (or enclosing block) via XData:
  - `GearID`, `teeth`, `module`, `pa`, `type`, etc.
- `SGEARSNAP`:
  - Fetch metadata for both gears.
  - Calculate center distance.
  - Move second gear’s block/objects.
  - Align angle by:
    - Picking a reference tooth on each gear (e.g., tooth with 0° starting flank).
    - Computing required rotation to align tooth/gap.

---

### 3.2 v1.2 — GUI (DCL or .NET Form) Layer

**Objective:** Provide a **clean, extensible GUI** that organizes options and can grow with future features.

#### 3.2.1 Features

- New command: `SGEARUI`
  - Opens a dialog with tabs:
    1. **Spur Gears**
    2. **Systems / Meshes**
    3. **Materials & Tolerances**
    4. **Advanced** (profile shift, backlash, etc.)
- Key GUI elements:
  - Dropdowns for material presets (Steel, PLA, MDF, Acrylic).
  - Radio buttons for module vs DP input.
  - Checkboxes for:
    - Draw pitch/base circles
    - Draw as block vs raw entities
  - Preview area (optional, v1.2 can skip preview if complex).

#### 3.2.2 Implementation Notes

- Separate UI definition into `sg-ui.dcl` and `sg-ui.lsp`.
- Make GUI simply **collect parameters and call existing core functions** (MVP code reused).
- Plan GUI structure with reserved sections for:
  - Internal gears (v1.3)
  - Planetary systems (v1.5)
  - Non-circular gears (v2.x)

---

### 3.2.1 v1.2.1 — Gear Animation

**Objective:** Animate gear systems, given an input RPM and direction, and propagate motion to all meshed gears.

#### 3.2.1.1 Features

- Command: `SGEARANIM`
  - User selects:
    - A gear (as “driver”).
    - Input RPM (positive = CCW, negative = CW).
  - User selects:
    - Gears that are meshed (or references an existing `GearSystem`).
  - System computes:
    - RPM and direction for each driven gear based on:
      (omega2 = -omega1 * T1 / T2)
    - For compound and planetary arrangements, support gear graphs:
      - Nodes = gear instances.
      - Edges = mesh relations (with sign and ratio).
  - Animate:
    - By applying incremental rotations to entities inside a loop.
    - Optionally store system as a named “animation scene” for repeated playback.

#### 3.2.1.2 Implementation Notes

- Represent gear meshes as adjacency lists:
  ```lisp
  (setq system
        (list
          (list 'gears (list g1 g2 g3 ...))
          (list 'meshes (list (list g1 g2) (list g2 g3) ...))))
  ```
- For animation:
  - Compute rotation angle per frame: theta = omega * delta_t
  - Use `ROTATE` or ActiveX transform calls in a loop.
  - Provide a way to **stop** animation (ESC or a dedicated stop command).

---

### 3.3 v1.3 — Internal Spur Gears

**Objective:** Extend spur gear generator to support **internal involute gears** (ring gears).

#### 3.3.1 Features

- `SGEARMAKE` gains a `type` parameter:
  - `:external` (default)
  - `:internal` (ring gear)
- Geometry logic:
  - Internal gear is an inversion of external teeth with modified addendum/dedendum.
  - Same base involute, but tooth profile mirrors relative to pitch circle.
- Integration:
  - `SGEARSNAP` extended to:
    - Snap external ↔ internal gear pairs.
    - Compute center distances:
      (C = m * (T_internal - T_external) / 2)

---

### 3.4 v1.4 — Tolerances & Material Presets

**Objective:** Make gears **laser-cut-ready** with material-aware tolerances.

#### 3.4.1 Features

- `SGEARMAT` command or GUI tab:
  - Presets:
    - Steel (CNC)
    - PLA (3D print)
    - MDF (laser)
    - Acrylic (laser)
  - For each:
    - Suggested tooth clearance, backlash, bore oversize, kerf compensation.
- Data model:
  - Material struct:
    ```lisp
    (list (cons 'name "MDF")
          (cons 'kerf 0.15)      ; mm
          (cons 'bore-oversize 0.2)
          (cons 'backlash 0.05))
    ```
- Geometry layer:
  - Given `tolerance` and `kerf`, offset tooth profiles outward/inward.
  - Adjust center distances slightly for wood/laser applications.

#### 3.4.2 UX

- When user starts typing a material:
  - Show inline suggestions: `PLA: 20` (for PA 20°) or `MDF: kerf 0.15` etc.
- Provide a “Recommend” button:
  - Inspects tooth count, size, and material → suggests DP/module and tolerances.

---

### 3.5 v1.5 — Planetary Gear Systems

**Objective:** Support creation of **sun–planet–ring** configurations with correct geometry and center distances.

#### 3.5.1 Features

- Command: `SGEARPLANET`
  - Inputs:
    - Sun teeth (`Ts`)
    - Planet teeth (`Tp`)
    - Ring teeth (`Tr`)
    - Number of planets
    - Module/DP and PA
  - Validates:
    - Planetary geometry constraints: Tr = Ts + 2 * Tp (for standard evenly spaced planets)
  - Output:
    - Sun gear at center.
    - Ring gear (internal).
    - Planets positioned at evenly spaced angles, correct center distances.
  - Optional:
    - Generate a carrier plate outline.

- Integration:
  - `SGEARANIM` should understand planetary systems:
    - If ring is fixed → relation between sun and carrier.
    - If carrier is input → relation between sun and ring.

---

## 4. Version 2.x — Advanced & Fun Features

### 4.1 v2.0 — Non-Circular Gears (Elliptical, Nautilus)

**Objective:** Support generation of **parametric non-circular gears**, starting with ellipses and nautilus/spiral-based profiles.

#### 4.1.1 Features

- `SGEARNC` command:
  - Mode options:
    - Elliptical
    - Nautilus (spiral step-change)
    - Other parametric shapes (future)
  - Parameters:
    - For elliptical:
      - Major/minor axes, center, orientation.
      - Tooth count or equivalent parameterization.
    - For nautilus:
      - Base radius
      - Radial growth per tooth
      - Teeth count.

- Geometry:
  - Approximate tooth flanks along normal directions of the parametric curve.
  - Ensure pitch length consistency (arc length along curve ≈ constant per tooth).

- Outputs:
  - Closed polyline representing non-circular gear.
  - Metadata linking to partner gear requirement (for conjugate design).

---

### 4.2 v2.1 — Convert Any Shape to Gear & Generate a Matching Driver

**Objective:** Allow the user to **select any closed polyline or region** and turn it into a gear, then auto-generate an approximate conjugate gear that will drive it.

#### 4.2.1 Features

- Command: `SGEARSHAPEPAIR`
  - User selects a closed shape (arbitrary).
  - User specifies:
    - Approximate tooth count or tooth pitch.
    - Module/DP, PA (if applicable).
  - System:
    - Samples the shape as a curve.
    - Places tooth “slots” along the curve based on desired pitch.
    - Constructs tooth flanks oriented along local normals.
    - Generates second gear (conjugate) that maintains near-constant velocity ratio along contact path.

- Constraints:
  - This is approximate; highlight “experimental” nature in docs.
  - Provide options for:
    - Point sampling density.
    - Smoothing / spline fitting.

---

## 5. Implementation Guidelines & Coding Best Practices

### 5.1 AutoLISP Structure

- Organize into multiple files:
  - `sg-math.lsp` — pure geometry and math utilities.
  - `sg-model.lsp` — gear objects, systems, validation.
  - `sg-acad.lsp` — DWG entity creation, transforms, layers.
  - `sg-ui.lsp` + `sg-ui.dcl` — UI / dialog.
  - `sg-anim.lsp` — animation logic.
  - `sg-planet.lsp` — planetary-specific logic.
- Provide a single **loader** file:
  - `sg-load.lsp` that `(load "...")` all modules and registers commands.

### 5.2 Data & Metadata

- Store gear metadata using:
  - XData or extension dictionaries on the gear’s main polyline or block.
- Always include:
  - `id`, `type`, `teeth`, `module`, `pa`, `bore`, `material`, `tolerance`, `system-id` (if part of a system).
- Make functions to convert between entity ID ↔ `Gear` object.

### 5.3 Testing & Debugging

- Provide debug utilities:
  - `SGEARDEBUG` to draw pitch/base circles, contact lines, etc.
  - Unit-test style functions in LISP to check:
    - Base radius, pitch diameter, involute point generator.

---

## 6. Documentation & Help

- Maintain a `HELP.md` or inline command:
  - `SGEARHELP` to list:
    - All commands.
    - Example usage.
    - Recommended presets (“Laser cutting MDF: use module X, kerf Y, tolerance Z”).

- For each version milestone:
  - Update `CHANGELOG.md` with:
    - New features.
    - Breaking changes.
    - Example DWG screenshots (references only).

---

## 7. Summary of Version Milestones

- **v1.0** — External involute spur gears (MVP).
- **v1.1** — Automatic gear snapping (center distance + tooth alignment).
- **v1.2** — Extensible GUI (DCL or .NET form), still for spurs only.
- **v1.2.1** — Gear animation: RPM propagation, visual rotation.
- **v1.3** — Internal spur gears (ring gears), updated snapping & animation.
- **v1.4** — Tolerances & material presets (laser / 3D print / CNC ready).
- **v1.5** — Planetary gear systems: sun/ring/planets with correct spacing + animation.
- **v2.0** — Non-circular gears (elliptical, nautilus).
- **v2.1** — Arbitrary-shape → gear + generated conjugate driver gear.

This roadmap describes both **the target vision** and **a staged, extensible path** for an AI coding agent to follow, ensuring early versions (MVP) are implemented in a way that smoothly scales to advanced features like planetary layouts, animations, and non-circular gear designs.
