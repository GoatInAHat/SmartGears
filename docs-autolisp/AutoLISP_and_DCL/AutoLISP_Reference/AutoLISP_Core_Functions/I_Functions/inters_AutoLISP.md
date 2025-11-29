---
title: inters (AutoLISP)
guid: "GUID-A181D474-F817-4550-86E9-87649262FA8A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A181D474-F817-4550-86E9-87649262FA8A.htm"
generated: "2025-11-28T19:06:33.402321Z"
description: Finds the intersection of two lines
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

# inters (AutoLISP)

> Finds the intersection of two lines

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A181D474-F817-4550-86E9-87649262FA8A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A181D474-F817-4550-86E9-87649262FA8A.htm)
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
(inters
pt1 pt2 pt3 pt4 [onseg]
)
```

- ***pt1*:** **Type:**  List  One endpoint of the first line.
- ***pt2*:** **Type:**  List  The other endpoint of the first line.
- ***pt3*:** **Type:**  List  One endpoint of the second line.
- ***pt4*:** **Type:**  List  The other endpoint of the second line.
- ***onseg*:** **Type:**  List or nil  If specified as `nil`, the lines defined by the four *pt*  arguments are considered infinite in length. If the *onseg*  argument is omitted or is not `nil`, the intersection point must lie on both lines or `inters`  returns `nil`.

## Return Values

**Type:**  List or nil

If the *onseg*  argument is present and is `nil`, `inters`  returns the point where the lines intersect, even if that point is off the end of one or both of the lines. If the *onseg*  argument is omitted or is not `nil`, the intersection point must lie on both lines or `inters`  returns `nil`. The `inters`  function returns `nil`  if the two lines do not intersect.

## Remarks

All points are expressed in terms of the current UCS. If all four point arguments are 3D, `inters`  checks for 3D intersection. If any of the points are 2D, `inters`  projects the lines onto the current construction plane and checks only for 2D intersection.

## Examples

```lisp
(setq a '(1.0 1.0) b '(9.0 9.0))
(setq c '(4.0 1.0) d '(4.0 2.0))

(inters a b c d)

nil

(inters a b c d T)

nil

(inters a b c d nil)

(4.0 4.0)
```
