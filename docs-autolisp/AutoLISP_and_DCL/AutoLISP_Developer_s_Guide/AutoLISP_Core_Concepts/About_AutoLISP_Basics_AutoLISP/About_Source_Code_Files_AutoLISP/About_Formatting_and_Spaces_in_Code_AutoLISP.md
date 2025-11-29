---
title: About Formatting and Spaces in Code (AutoLISP)
guid: "GUID-2BB34AE2-0AA1-4523-913E-1438A9B7BF35"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2BB34AE2-0AA1-4523-913E-1438A9B7BF35.htm"
generated: "2025-11-28T19:06:02.189431Z"
description: AutoLISP code can span multiple lines, and contain empty lines and extra spaces. Empty lines and extra spaces do not have any significant meaning, but can make your code easier to read.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# About Formatting and Spaces in Code (AutoLISP)

> AutoLISP code can span multiple lines, and contain empty lines and extra spaces. Empty lines and extra spaces do not have any significant meaning, but can make your code easier to read.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2BB34AE2-0AA1-4523-913E-1438A9B7BF35.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2BB34AE2-0AA1-4523-913E-1438A9B7BF35.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Multiple spaces between function and variable names, and constants are equivalent to a single space. The end of a line and tab is also treated as a single space. The following two expressions produce the same result:

```lisp
(setq test1 123 test2 456)

(setq
  test1 123
  test2 456
)
```

The extensive use of parentheses in AutoLISP code can make it difficult to read. The traditional techniques for combatting this confusion is indentation, and to align the open and close parentheses of a function. The more deeply nested a line of code is, the farther to the right the line is positioned.

The following two functions are the same code, but the second one is much easier to read and determine visually if the parentheses of the AutoLISP expressions are balanced.

```lisp
(defun c:mycmd ()
(setq old_clayer (getvar "clayer"))
(setq insPt (getpoint "\nSpecify text insertion: "))
(if (/= insPt nil)
(progn
(command "_.UNDO" "_BE")
(command "._-LAYER" "_M" "Text" "_C" "3" "" "")
(command "_.-TEXT" insPt "" "0" "Sample Text")
(command "_.UNDO" "_E")))
(setvar "clayer" old_clayer)
(princ)
)

(defun c:mycmd ()
  (setq old_clayer (getvar "clayer"))

  (setq insPt (getpoint "\nSpecify text insertion: "))

  (if (/= insPt nil)
    (progn
      (command "_.UNDO" "_BE")
      (command "._-LAYER" "_M" "Text" "_C" "3" "" "")
      (command "_.-TEXT" insPt "" "0" "Sample Text")
      (command "_.UNDO" "_E")
    )
  )

  (setvar "clayer" old_clayer)
 (princ)
)
```
