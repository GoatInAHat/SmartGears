---
title: Completing a Word Automatically
guid: "GUID-A17D00D1-9784-4B7E-B222-82176CB44E4F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A17D00D1-9784-4B7E-B222-82176CB44E4F.htm"
generated: "2025-11-28T19:07:02.471633Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Completing a Word Automatically

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A17D00D1-9784-4B7E-B222-82176CB44E4F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A17D00D1-9784-4B7E-B222-82176CB44E4F.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Imagine you are adding some new functionality to your program using the following code:

```lisp
ObjectCreationStyle (strcase (cdr (assoc 3 BoundaryData)))

(if (equal ObjectCreationStyle  "COMMAND")
 (progn
  (setq firstCenterPt (polar rowCenterPt (Degrees->Radians 45) distanceOnPath))
    (gp:Create_activeX_Circle)
  )
)
```

(Do not worry about what this code actually does, if anything. It is only an example that includes several long variable and function names.)

Visual LISP can save you some keystrokes by completing words for you.

## To use the Visual LISP Complete Word by Match feature

1.  Scroll to the bottom of the
   gpdraw.lsp
    file and enter the following code:

   ```lisp
   ObjectCreationStyle (strcase (cdr (assoc 3 BoundaryData)))
      (if (equal Ob
   ```
2. Press Ctrl+Spacebar.

   Visual LISP just saved you seventeen keystrokes as it searched within the current file and found the closest match to the last two letters you typed.
3. Complete the line of code so that it looks like the following:

   ```lisp
   (if (equal ObjectCreationStyle "COMMAND")
   ```
4. Add the following lines:

   ```lisp
   (progn
     (setq firstCenterPt (p
   ```
5. Press Ctrl+Spacebar.

   Visual LISP matches the most recent “p” word, which happens to be `progn`. However, the word you need is `polar`. If you keep pressing Ctrl+Spacebar., Visual LISP cycles through all the possible matches in your code. Eventually, it will come around to `polar`.
6. Delete all the code you just entered; it was for demonstration purposes only.

   The Complete Word by Match feature is also available from the Visual LISP Search menu.
