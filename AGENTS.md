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

## 8.2 Offline Documentation System & Mandatory Usage Rules

- **Location & immutability**: Offline docs live in `docs-autolisp/` with assets in `_assets` and the lookup index in `_navmap.json`. Never modify, reformat, or delete anything inside `docs-autolisp/`; regenerate only via `scripts/build_offline_autolisp_docs.py` (Python 3 + `beautifulsoup4` required).
- **Documentation map (mirrors folder hierarchy)**  
  - `AutoLISP_and_DCL/AutoLISP_Developer_s_Guide/` – Core concepts (namespaces, variables, data types, lists, error handling), user input, selection sets, geometric utilities, entity data/handles, XData and dictionaries, file handling, display control, ActiveX, DCL basics, VS Code/Visual LISP IDE workflows.  
  - `AutoLISP_and_DCL/AutoLISP_Reference/` – Function catalogs for arithmetic, strings, lists, equality/conditionals, selection sets, object/entity manipulation, entmake/entmod, command/query functions, user input, geometric utilities, namespaces, registry/property lists, file I/O, extended data, symbol tables/dictionaries, ActiveX library calls, DCL reference, Express Tools, and Visual LISP IDE utilities.  
  - `AutoLISP_and_DCL/AutoLISP_Tutorials/` – Guided walkthroughs (Garden Path reactor project), AutoLISP extension tutorials, and foundational lessons on reactors, geometry, dialog creation, and debugging.  
- **Usage mandates (apply all of these)**  
  - Open relevant offline pages before writing AutoLISP code or selecting any AutoLISP/DCL/ActiveX function.  
  - Consult docs before implementing math or geometry routines, entity construction (`entmake`, polylines, circles, splines), or any command/selection-set interaction.  
  - Reference docs when designing data structures, handling strings/lists, or sanitizing user input.  
  - Check docs prior to any AutoCAD interaction (commands, system variables, reactors) and when generating gear geometry.  
  - During debugging or error triage, re-read the APIs involved (functions reference + guide sections) before making changes.  
  - Load the relevant pages into the context window at the start of every task.  
  - When submitting a PR or summary, explicitly name the offline pages consulted (use `_navmap.json` paths).  
- **Rationale**: AutoLISP is niche, official references are sparse online, and LLM hallucination risk is high. Frequent, explicit consultation of the offline set is mandatory to avoid invalid code or mis-remembered function behaviors.

---

## 8.5 Test Success Requirement

All tasks must conclude with a verification step:
- All existing tests must pass.
- All newly added tests relevant to the current task must pass.
- The agent must only output: "Ready to submit PR!" (or "Ready to update branch!", depending on context) when all tests pass and no unresolved issues remain.
- If any test fails, the agent must not finalize work and must continue debugging.

## 8.6 Bug Resolution Protocol (Strict TDD)

When a bug is reported, follow this exact workflow:

0. Read the docs first.  
   - Before writing a new test, open the applicable pages in `docs-autolisp/` (guide + function reference) to confirm expected behavior and edge cases.

1. Write a failing test first.
   - Before attempting any fix, create a test that reproduces the bug.
   - Run the full test suite and confirm that the newly added test fails.

2. Iteratively fix the bug.
   - Apply a small change.
   - Run the entire test suite after each revision.

3. Failure threshold.
   - If the relevant test fails more than 3 consecutive times, automatically:
     - Revert all recent changes.
     - Re‑examine whether the test is correct.
     - If the test appears flawed, revise it and restart the cycle.

4. Success condition.
   - Once the previously failing test passes and the full suite passes with no regressions, the bug is considered fixed.

5. Finalization.
   - Update documentation if needed.
   - Only then should the agent output: "Ready to submit PR!".

---

## 9. Summary: Core Imperatives  

- Always treat documentation as first-class — read before writing.  
- Always test before committing.
- Separate concerns — keep code modular.
- Document assumptions and references.
- Manage context window actively to avoid noise, repetition, and hallucination drift.
- Seek human input when uncertain.

### CI/Release Guardrail
- Do not disable, bypass, or remove GitHub Actions steps that bundle sources, run smoke tests, or publish draft releases; keep the automated build-and-test pipeline intact when modifying workflows.

### Status reporting in final responses
- Always include an explicit readiness statement in the final summary (e.g., "Ready to push" or "Not ready to push").
- If not ready, list the blockers.

If at any point you can’t satisfy these conditions, **abort or request review** before proceeding.
