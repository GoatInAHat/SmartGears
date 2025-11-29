---
title: grdraw (AutoLISP)
guid: "GUID-C00544A7-AF63-403E-98EB-205248EB4F54"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C00544A7-AF63-403E-98EB-205248EB4F54.htm"
generated: "2025-11-28T19:06:32.026208Z"
description: Draws a vector between two points, in the current viewport
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
  - function
---

# grdraw (AutoLISP)

> Draws a vector between two points, in the current viewport

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C00544A7-AF63-403E-98EB-205248EB4F54.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C00544A7-AF63-403E-98EB-205248EB4F54.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(grdraw
from to color [highlight]
)
```

- ***from*:** **Type:**  List  2D or 3D points (lists of two or three reals) specifying one endpoint of the vector in terms of the current UCS. AutoCAD clips the vector to fit the screen.
- ***to*:** **Type:**  List  2D or 3D points (lists of two or three reals) specifying the other endpoint of the vector in terms of the current UCS. AutoCAD clips the vector to fit the screen.
- ***color*:** **Type:**  Integer  Color used to draw the vector. A -1 signifies *XOR ink*, which complements anything it draws over and which erases itself when overdrawn.
- ***highlight*:** **Type:**  Integer  Other than zero, indicates that the vector is to be drawn using the default highlighting method of the display device (usually dashed).  If *highlight*  is omitted or is zero, `grdraw`  uses the normal display mode.

## Return Values

**Type:**  nil

Always returns `nil`.

## Examples

N/A
