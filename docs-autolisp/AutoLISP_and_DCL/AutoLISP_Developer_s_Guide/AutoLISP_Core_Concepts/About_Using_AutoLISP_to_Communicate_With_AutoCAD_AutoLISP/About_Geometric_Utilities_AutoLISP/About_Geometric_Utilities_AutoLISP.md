---
title: About Geometric Utilities (AutoLISP)
guid: "GUID-56A7AF2E-F3CC-45B3-915C-A34290A591DA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-56A7AF2E-F3CC-45B3-915C-A34290A591DA.htm"
generated: "2025-11-28T19:06:08.109341Z"
description: A group of functions allows applications to obtain pure geometric information and geometric data from the drawing.
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

# About Geometric Utilities (AutoLISP)

> A group of functions allows applications to obtain pure geometric information and geometric data from the drawing.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-56A7AF2E-F3CC-45B3-915C-A34290A591DA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-56A7AF2E-F3CC-45B3-915C-A34290A591DA.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The following lists some of the commonly used geometric related functions:

- angle
   – Returns the angle, in radians, between a line and the
  X
   axis (of the current UCS).
- distance
   – Returns the distance between two points.
- polar
   – Locates a point by means of polar coordinates (relative to an initial point).
- inters
   – Locates the intersection point of two lines.
- osnap
   – Returns a 3D point that is the result of applying an Object Snap mode to a specified point.
- textbox
   – Measures a specified text object, and returns the diagonal coordinates of the bounding box that encloses the text.

The following code example demonstrates calls to the geometric utility functions:

```lisp
(setq pt1 '(3.0 6.0 0.0))
(setq pt2 '(5.0 2.0 0.0))
(setq base '(1.0 7.0 0.0))
(setq rads (angle pt1 pt2))   ; Angle in XY plane of current UCS - value
                              ; is returned in radians

(setq len (distance pt1 pt2)) ; Distance in 3D space
(setq endpt (polar base rads len))
```

The call to `polar`  sets `endpt`  to a point that is the same distance from (1,7) as `pt1`  is from `pt2`, and at the same angle from the *X*  axis as the angle between `pt1`  and `pt2`.
