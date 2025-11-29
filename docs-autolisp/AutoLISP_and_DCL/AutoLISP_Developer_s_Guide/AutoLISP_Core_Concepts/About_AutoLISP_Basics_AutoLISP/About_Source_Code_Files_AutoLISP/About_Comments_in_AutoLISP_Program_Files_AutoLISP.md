---
title: About Comments in AutoLISP Program Files (AutoLISP)
guid: "GUID-4D4664AD-301F-4E6E-AD65-4B7CE6A258B8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4D4664AD-301F-4E6E-AD65-4B7CE6A258B8.htm"
generated: "2025-11-28T19:06:02.335461Z"
description: Comments are useful to both the programmer and future users who may need to revise a program to suit their needs.
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

# About Comments in AutoLISP Program Files (AutoLISP)

> Comments are useful to both the programmer and future users who may need to revise a program to suit their needs.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4D4664AD-301F-4E6E-AD65-4B7CE6A258B8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4D4664AD-301F-4E6E-AD65-4B7CE6A258B8.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

It is good coding practice to include comments in all AutoLISP program files, small and large. Use comments to do the following:

- Give a title, authorship, and creation date
- Provide instructions on using a routine
- Make explanatory notes throughout the body of a routine
- Make notes to yourself during debugging

Comments begin with one or more semicolons (**`;`**) and continue through the end of the line.

```lisp
; This entire line is a comment
(setq area (* pi r r)) ; Compute area of circle
```

Any text within **`;| ... |;`**  is ignored. Therefore, comments can be included within a line of code or extend for multiple lines. This type of comment is known as an in-line comment.

```lisp
(setq tmode ;|some note here|; (getvar "tilemode"))
```

The following example shows a comment that continues across multiple lines:

```lisp
(setvar "orthomode" 1) ;|comment starts here
and continues to this line,
but ends way down here|; (princ "\nORTHOMODE set On.")
```
