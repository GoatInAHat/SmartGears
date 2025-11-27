# AGENTS.md — Agent Guidelines for SmartGears

This file provides instructions and expectations for any AI agent (e.g. OpenAI Codex or similar) contributing to **SmartGears**.  
It is intended to ensure safety, reliability, maintainability, and clarity in all AI-driven code modifications.

---

## 0. Purpose & Rationale

- SmartGears uses **niche technologies and domains** — e.g. AutoLISP for AutoCAD automation, involute-gear geometry math, laser-cut tolerancing — which have very limited examples and public code. 
- Large-language models tend to **hallucinate or misuse APIs** when resources are rare or ambiguous.  
- To avoid generating incorrect, fragile or unmaintainable code, the agent must treat **official documentation and credible external sources as first-class references**.  
- Use of external documentation and repeated validation/testing is not optional — it is **mandatory**.
- Keep everything compatible with all versions of AutoCAD, including AutoCAD web.
- Ensure compatibility with AutoCAD Web: prioritize use of features supported by AutoCAD Web (e.g., avoid File Dialogs, ActiveX, or unsupported UI features).

---

## 1. Key Documentation Resources (Always in Context)

When writing or modifying AutoLISP, geometry, CAD output, or gear-generation logic, the agent must keep the following (at minimum) available and reference them explicitly when using relevant features:

- AutoLISP Developer's Guide — the core reference for AutoLISP / Visual LISP functions and usage (this is the holy grail, it should always be kept in the context window and referenced frequently).  
  <https://help.autodesk.com/view/OARX/2025/ENU/?guid=GUID-265AADB3-FB89-4D34-AA9D-6ADF70FF7D4B>  [oai_citation:0‡Autodesk Help](https://help.autodesk.com/view/OARX/2025/ENU/?guid=GUID-265AADB3-FB89-4D34-AA9D-6ADF70FF7D4B&utm_source=chatgpt.com)  
- AutoLISP / Visual LISP functions reference for geometry, entity creation, list operations, file I/O. <https://help.autodesk.com/view/OARX/2025/ENU/?guid=GUID-4CEE5072-8817-4920-8A2D-7060F5E16547>  [oai_citation:1‡Autodesk Help](https://help.autodesk.com/view/OARX/2025/ENU/?guid=GUID-4CEE5072-8817-4920-8A2D-7060F5E16547&utm_source=chatgpt.com)  
- Any external reference used for gear-geometry math, involute curves, tolerancing, or laser-cut kerf compensation.  
- When using tutorials or community code — ensure at least two independent credible sources, compare for consistency, and verify via tests.  

The agent should **always keep these references in its context window** when working on any code related to AutoLISP, geometry generation, or CAD output.  

The agent must also consider AutoCAD Web limitations as documented in AutoCAD Web LISP support pages.

---

## 2. Workflow: Research → Code → Test → Document → Commit  

For each task (feature, bug fix, refactor), follow this pipeline exactly:

1. **Research & Documentation Gathering**  
   - Use the web-access tool to search for relevant docs (official guides, geometry math references, CAD best-practice).  
   - Collect **at least two credible sources**, read completely, and extract relevant rules / constraints / edge-cases.  
   - Embed a concise summary or quote (with URL) in code comments or doc files when relying on external behavior or formulas.

2. **Plan & Outline Implementation**  
   - Sketch pseudocode or design notes if algorithmic or structural complexity is non-trivial (e.g. internal-gear geometry, kerf/tolerance handling, planetary spacing).  

3. **Implement Code (modular)**  
   - Strictly separate concerns per architecture (see ROADMAP.md): math logic in geometry modules; CAD drawing in integration modules; UI or I/O separate.  
   - Avoid global state or side effects in core logic; use well-structured data (association lists or struct-like alists) for gear metadata.  

4. **Test Thoroughly**  
   - Write unit tests or minimal deterministic tests covering nominal and edge cases (e.g. minimal tooth counts, zero clearance, invalid inputs).  
   - Tests must pass without exception before any commit.  
   - If graphics / CAD output is involved, include "smoke tests" (e.g. load, draw, ensure no syntax or runtime errors).  

5. **Document Changes**  
   - Update user-facing docs (`docs/README.md`, HELP sections) when adding or changing functionality.  
   - Embed references in comments for unusual behavior, assumptions, or approximations (e.g. kerf values, clearance).  

6. **Commit & Versioning**  
   - Use small, focused commits.  
   - Commit message must begin with milestone prefix (e.g. `v1.0: …`, `v1.3: …`).  
   - Branch naming should reflect milestone and feature (e.g. `feature/v1.0-involute`, `bugfix/v1.1-snap-align`).  

7. **Human-In-Loop & Safety Checks**  
   - If uncertain about parameters, geometry feasibility, or tolerance values — **do not guess**. Pause and request human input.  
   - Avoid destructive operations (e.g. deleting existing files or data) without explicit instructions.  

---

## 3. Context-Window Management (Very Important)

Because context is limited and repetitive logs, debug output, or irrelevant history can degrade performance, the agent must manage its context actively:

- Before beginning a task: **load relevant docs** (AutoLISP guide, geometry references, gear-math resources) into the context window.  
- When bugs or errors arise: **do not** repeatedly dump stacks or incremental output. Instead, **clear irrelevant context**, re-load documentation, and re-approach—especially for subtle logic or geometry bugs.  
- If stuck in a loop (e.g. failing tests repeatedly without clear fix): **pause and request human review** rather than prolonging guesswork.  

This ensures the agent does not accumulate noise or drift into hallucination territory.  

---

## 4. Code & Style Conventions

- Prefix all public command-names with `SGEAR…` (e.g. `SGEARMAKE`, `SGEARSNAP`).  
- All core math functions must be **pure** (no side-effects, no AutoCAD calls).  
- Use **association lists (alist)** or equivalent structured data for gear and system metadata (teeth count, module, bore, type, material, tolerance, etc.).  
- Internal numeric units must be in **millimeters** unless explicitly documented otherwise.  
- Functions should include header comments: purpose, inputs, outputs, side-effects (if any), preconditions, units.  
- Avoid “magic numbers” — parameterize or document them.  

---

## 5. Testing & Quality Assurance Requirements  

- **Test-Driven Development (TDD)** is mandatory. No untested code should be committed.  
- Tests must cover normal cases, edge cases, and invalid inputs.  
- For geometry / CAD output: include basic smoke tests (e.g. geometry generation doesn’t crash, valid polylines, correct bounding diameters, etc.).  
- All tests must pass before merging or committing.  

---

## 6. Use of External Documentation & References  

Whenever using external algorithms or behaviors (e.g. involute gear math, gear-mesh formulas, kerf/tolerance offsets), the agent must:

- Fetch the original reference (document, standard, or credible community source).  
- Verify via tests for correctness (e.g. expected pitch diameter, correct tooth spacing, internal-gear clearance).  
- Cite the reference (URL or document name) in code comments or documentation.  
- If parameters are approximate (e.g. kerf width, material clearance), mark them explicitly as “recommended” with comment, not hard-coded assumptions.  

---

## 7. Collaboration Rules & PR Guidelines  

- Each Pull Request must:  
  - Reference the related Issue or roadmap milestone.  
  - Include a clear summary of changes, added/updated tests, documentation updates.  
  - Show that tests pass, code style is respected, no leftover debug output, and commit message uses correct prefix.  
- If change is large or complex (geometric math, tolerances, non-trivial algorithms), include explanation and reasoning (with references) in PR description for human review.  

---

## 8. When to Ask for Human Input  

The agent must stop and ask for human review in the following situations:

- When unclear about real-world parameters (e.g. material kerf, tolerance for laser-cut MDF, screw clearance).  
- When a bug or test failure stems from uncertain or ambiguous geometry or CAD behavior.  
- When making design decisions with potential aesthetic or usability consequences (e.g. UI layout, user-facing defaults, mechanical tolerances for wood).  

Do **not** guess or assume — ask.  

---

## 8.1 GitHub Actions: Single-File Build for AutoCAD Web

SmartGears will use a GitHub Action to automatically bundle all modular .lsp source files into a single distribution-ready SmartGears.lsp file to maximize AutoCAD Web compatibility. The agent must ensure code remains modular internally while preserving the ability to concatenate modules in the correct order for a final single-file build. When modifying or adding modules, the agent must verify that the bundling script remains correct and that the output SmartGears.lsp loads successfully in both AutoCAD Desktop and AutoCAD Web.

---

## 9. Summary: Core Imperatives  

- Always treat documentation as first-class — read before writing.  
- Always test before committing.  
- Separate concerns — keep code modular.  
- Document assumptions and references.  
- Manage context window actively to avoid noise, repetition, and hallucination drift.  
- Seek human input when uncertain.  

If at any point you can’t satisfy these conditions, **abort or request review** before proceeding.  