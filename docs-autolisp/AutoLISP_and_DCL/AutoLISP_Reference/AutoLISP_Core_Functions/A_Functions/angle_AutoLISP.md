---
title: angle (AutoLISP)
guid: "GUID-F28755D4-E89F-43EF-8E76-40518C7E2728"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F28755D4-E89F-43EF-8E76-40518C7E2728.htm"
generated: "2025-11-28T19:06:22.779707Z"
description: Returns an angle in radians of a line defined by two endpoints
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

# angle (AutoLISP)

> Returns an angle in radians of a line defined by two endpoints

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F28755D4-E89F-43EF-8E76-40518C7E2728.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F28755D4-E89F-43EF-8E76-40518C7E2728.htm)
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
(angle
pt1 pt2
)
```

- ***pt1*:** **Type:**  Integer or Real  An endpoint.
- ***pt2*:** **Type:**  Integer or Real  An endpoint.

## Return Values

**Type:**  Real

An angle, in radians.

The angle is measured from the *X*  axis of the current construction plane, in radians, with angles increasing in the counterclockwise direction. If 3D points are supplied, they are projected onto the current construction plane.

## Examples

```lisp
(angle '(1.0 1.0) '(1.0 4.0))

1.5708

(angle '(5.0 1.33) '(2.4 1.33))

3.14159
```
