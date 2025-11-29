---
title: "About Controlling Low-Level Graphics (AutoLISP)"
guid: "GUID-57942DDA-8A41-4456-B021-FDEF0475A6AE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-57942DDA-8A41-4456-B021-FDEF0475A6AE.htm"
generated: "2025-11-28T19:06:07.534218Z"
description: "Low-level graphics in the drawing area and application window can be controlled using AutoLISP functions."
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

# About Controlling Low-Level Graphics (AutoLISP)

> Low-level graphics in the drawing area and application window can be controlled using AutoLISP functions.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-57942DDA-8A41-4456-B021-FDEF0475A6AE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-57942DDA-8A41-4456-B021-FDEF0475A6AE.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The following functions can be used to draw temporary objects in the drawing area or display text in the status bar:

- grtext
   – Displays text directly in the status bar area, with or without highlighting.
  Note:
   This function is supported on Mac OS, but does not affect AutoCAD.
- grdraw
   – Draws a temporary vector in the current viewport with control over color and highlighting.
- grvecs
   – Draws multiple temporary vectors.
- redraw
   – Redraws the current viewport or a specified object (entity) in the current viewport.

Note:
 Because these functions depend on code in AutoCAD, their operation can be expected to change from release to release. There is no guarantee that applications calling these functions will be upward compatible. Also, they depend on current hardware configurations. In particular, applications that call
grtext
 are not likely to work the same on all configurations unless the developer is very careful to use them as described and to avoid hardware-specific features. Finally, because they are low-level functions, they do almost no error reporting and can alter the graphics screen display unexpectedly (see the following example for a way to fix this).

The following sequence restores the default graphics window display caused by incorrect calls to `grtext`, `grdraw`, or `grvecs`:

```lisp
(grtext) ; Restores standard text
(redraw)
```
