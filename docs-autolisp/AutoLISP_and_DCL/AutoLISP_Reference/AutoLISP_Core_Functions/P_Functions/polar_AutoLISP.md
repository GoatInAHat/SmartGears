---
title: polar (AutoLISP)
guid: "GUID-6A84BFD3-8788-45B1-AB52-5E83F0C5286E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6A84BFD3-8788-45B1-AB52-5E83F0C5286E.htm"
generated: "2025-11-28T19:06:38.618176Z"
description: Returns the UCS 3D point at a specified angle and distance from a point
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

# polar (AutoLISP)

> Returns the UCS 3D point at a specified angle and distance from a point

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6A84BFD3-8788-45B1-AB52-5E83F0C5286E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6A84BFD3-8788-45B1-AB52-5E83F0C5286E.htm)
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
(polar
pt ang dist
)
```

- ***pt*:** **Type:**  List  A 2D or 3D point.
- ***ang*:** **Type:**  Integer or Real  An angle expressed in radians relative to the world *X*  axis. Angles increase in the counterclockwise direction, independent of the current construction plane.
- ***dist*:** **Type:**  Integer or Real  Distance from the specified *pt*.

## Return Values

**Type:**  List

A 2D or 3D point, depending on the type of point specified by *pt*.

## Examples

Supplying a 3D point to `polar`:

```lisp
(polar '(1 1 3.5) 0.785398 1.414214)

(2.0 2.0 3.5)
```

Supplying a 2D point to `**polar**`:

```lisp
(polar '(1 1) 0.785398 1.414214)

(2.0 2.0)
```
