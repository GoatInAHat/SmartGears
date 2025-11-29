---
title: distance (AutoLISP)
guid: "GUID-62F2AA98-1CE4-4E31-A2B7-E2C49A92E38F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-62F2AA98-1CE4-4E31-A2B7-E2C49A92E38F.htm"
generated: "2025-11-28T19:06:27.445445Z"
description: Returns the 3D distance between two points
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

# distance (AutoLISP)

> Returns the 3D distance between two points

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-62F2AA98-1CE4-4E31-A2B7-E2C49A92E38F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-62F2AA98-1CE4-4E31-A2B7-E2C49A92E38F.htm)
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
(distance
pt1 pt2
)
```

- ***pt1*:** **Type:**  List  A 2D or 3D point list.
- ***pt2*:** **Type:**  List  A 2D or 3D point list.

## Return Values

**Type:**  Real

The distance. If one or both of the supplied points is a 2D point, then `distance`  ignores the *Z*  coordinates of any 3D points supplied and returns the 2D distance between the points as projected into the current construction plane.

## Examples

```lisp
(distance '(1.0 2.5 3.0) '(7.7 2.5 3.0))

6.7

(distance '(1.0 2.0 0.5) '(3.0 4.0 0.5))

2.82843
```
