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

## 8.5 Test Success Requirement

All tasks must conclude with a verification step:
- All existing tests must pass.
- All newly added tests relevant to the current task must pass.
- The agent must only output: "Ready to submit PR!" (or "Ready to update branch!", depending on context) when all tests pass and no unresolved issues remain.
- If any test fails, the agent must not finalize work and must continue debugging.

## 8.6 Bug Resolution Protocol (Strict TDD)

When a bug is reported, follow this exact workflow:

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

If at any point you can’t satisfy these conditions, **abort or request review** before proceeding.  