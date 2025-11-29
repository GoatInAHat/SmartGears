---
title: Completing a Word by Apropos
guid: "GUID-FDB150B1-DD7A-4E11-989B-429A03EF8310"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-FDB150B1-DD7A-4E11-989B-429A03EF8310.htm"
generated: "2025-11-28T19:07:02.577136Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# Completing a Word by Apropos

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-FDB150B1-DD7A-4E11-989B-429A03EF8310.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-FDB150B1-DD7A-4E11-989B-429A03EF8310.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

If you have worked with AutoLISP before, you may have had to type in an expression similar to the one shown below:

```lisp
(setq myEnt (ssname mySelectionSet ssIndex))
```

Often, it is confusing to keep track of all the selection set functions: `ssname`, `ssget`, `sslength`, and so on. Visual LISP can help, using its Complete Word by Apropos feature.

## To use the Visual LISP Complete Word by Apropros feature

1.  Scroll to the bottom of the
   gpdraw.lsp
    file and enter the following on a blank line:

   ```lisp
   (setq myEnt (ent
   ```
2. Press Ctrl+Shift+Spacebar.

   Visual LISP displays a list of all AutoLISP symbols that begin with the letters *ent*.

   Use the cursor keys (the up and down arrow keys) to move through the list. Select `ENTGET`, then press Enter.

   Visual LISP replaces the `ent`  you typed with `ENTGET`.
3.  Delete the code.
