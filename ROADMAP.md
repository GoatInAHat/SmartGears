# Roadmap — SmartGears (AutoLISP, AutoCAD Web)

> A carefully ordered development plan for SmartGears.  
> All public commands begin with `SG`.  
> Every phase must preserve Web‑compatible AutoLISP, bundling integrity, and separation of math/geom vs AutoCAD adapters.

---

## Phase 0 — Zero‑Input MVP (Complete)

**Command:** `SGMVP`  
Creates a 10‑tooth external spur gear at origin using fixed parameters:  
- module = 2 mm  
- PA = 20°  
- bore = 5 mm  
Draws pitch + base circles.  
Stores metadata.  
Tested for geometry, closure, bundling.

This must never regress.

---

## Phase 1 — Parameterized External Spur Gears (`SGSPUR`) (Complete)

**Command:** `SGSPUR`  
Creates a configurable external spur gear with cascading defaults.

### Parameter Order (with default rules):
1. **Teeth** — required, integer ≥6, no default.  
2. **Module** — computed default based on teeth (keeps pitch diam. in target range).  
3. **Pressure angle** — default 20°.  
4. **Bore diameter** — computed default from pitch diameter (e.g., 15–25%).  
5. **Material/Tolerance preset** — default = current global preset.

### Details:
- All geometry in pure `geom-spur` module.
- All CAD creation in `acad-entities` module.
- Must compute contact ratio ≥1.4; warn on undercut.
- Store full metadata including active tolerance values.

### Tests:
- Unit tests for module/bore defaults.  
- Geometry regression for 10T, 20T, 40T spur gears.  
- Polyline closure + contact‑ratio tests.

---

## Phase 2 — External Spur Snapping (`SGSNAP`)

**Command:** `SGSNAP`  
Snaps two external spur gears into correct center distance & phase alignment.

### Inputs:
- Pick gear A  
- Pick gear B  
- Optional angle override  
- Optional clearance override (default from tolerance preset)

### Behavior:
- Center distance:  
  `C = m*(T1 + T2)/2 + clearance`
- Align tooth-to-gap phase.  
- Must be idempotent: repeated calls yield same placement.

### Tests:
- Snap 20T + 40T → verify correct center distance & orientation.  
- Repeatability tests.

---

## Phase 2.5 — Global Tolerances (`SGTOLERANCE`)

**Command:** `SGTOLERANCE`  
Sets one global tolerance scalar (mm), positive or negative.

### Inputs:
- Tolerance (mm)

### Behavior:
- Stored as global tolerance.  
- All new gears use this.  
- Optionally apply to existing gears.

### Tests:
- Persistence across commands.  
- SGSPUR defaults respond to new tolerance.

---

## Phase 3 — Internal Spur Gears (`SGINT`) + Internal Snapping

**Command:** `SGINT`  
Creates internal spur gears (inward-facing teeth) with cascading defaults.

### Parameters:
Same as SGSPUR with same default rules.

### Behavior:
- Implement internal involute rules.  
- Undercut & contact-ratio checks.

### SGSNAP Extension:
- Must support external-to-internal meshing:  
  `C = m*(T_int - T_ext)/2 + clearance`
- Phase alignment flips sign appropriately.

### Tests:
- Fixture tests for internal involute.  
- Snap external‑20T to internal ring.

---

## Phase 3.5 — Dual-Profile Ring Gears (`SGRING`)

**Command:** `SGRING`  
Creates a gear body with both **outer external teeth** and **inner internal teeth**.

### Parameters:
1. Outer teeth (required)  
2. Inner teeth (required)  
3. Module (computed for compatibility)  
4. PA (default 20°)  
5. Bore diameter (computed)  
6. Material/Tolerance preset

### Behavior:
- Validate module compatibility for both profiles.  
- Single CAD body with both profiles.  
- Metadata includes both tooth counts.

### SGSNAP Requirements:
- Snap external gears to outer teeth.  
- Snap external gears to inner teeth (internal mesh).

### Tests:
- Geometry fixtures.  
- Snapping tests for both sides.

---

## Phase 4 — Preset Non‑Circular Gears (`SGNAUTILUS`, `SGELLIPSE`)

### Command: `SGNAUTILUS`
Generates nautilus/log‑spiral‑type gears.

**Parameters:**
- Teeth (required)  
- Module (computed)  
- Base radius (computed)  
- Growth parameters (safe defaults)

### Command: `SGELLIPSE`
Generates elliptical gears using preset pitch curves.

**Parameters:**
- Major axis  
- Minor axis  
- Teeth (required)  
- Module (computed)

### Behavior:
- Fully Web‑safe geometry pipeline using arc‑length sampling.
- Use pure geometry modules for curve → teeth mapping.

### Tests:
- Closed-profile tests.  
- Collision tests.  
- Golden fixtures.

---

## Phase 5 — General Non‑Circular Gears (`SGNC`)

**Command:** `SGNC`  
General non‑circular gear generator.

### Inputs:
- Template (ellipse, lemniscate, nautilus) OR CSV polar import  
- Teeth  
- Module (computed)  
- Optional transmission ratio

### Behavior:
- Implement Xu 2020 pitch sampling → ψ integration → conjugate synthesis.  
- Validate geometric + torque constraints.  
- Store NC-type metadata.

### Tests:
- Fixtures for several NC pairs.  
- Transmission consistency tests.

---

## End of Phase 0–5

Next roadmap extension (systems, animation, planetary, optimization) happens only after Phase 5 completion.

---

# Always‑On Requirements
- All math/geom AutoCAD‑free.  
- All CAD calls isolated in acad modules.  
- Maintain bundling order.  
- Update and pass all tests per phase.  
- Never regress SGMVP functionality.