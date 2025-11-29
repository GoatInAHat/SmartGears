---
title: osnap (AutoLISP)
guid: "GUID-33498AA8-EDC0-45A3-9603-47A3F3510924"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33498AA8-EDC0-45A3-9603-47A3F3510924.htm"
generated: "2025-11-28T19:06:38.331635Z"
description: Returns a 3D point that is the result of applying an Object Snap mode to a specified point
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

# osnap (AutoLISP)

> Returns a 3D point that is the result of applying an Object Snap mode to a specified point

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33498AA8-EDC0-45A3-9603-47A3F3510924.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33498AA8-EDC0-45A3-9603-47A3F3510924.htm)
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
(osnap
pt mode
)
```

- ***pt*:** **Type:**  List  A point.
- ***mode*:** **Type:**  String  One or more valid Object Snap identifiers, such as `mid`, `cen`, and so on, separated by commas.

## Return Values

**Type:**  List or nil

A point; otherwise `nil`, if the pick did not return an object (for example, if there is no geometry under the pick aperture, or if the geometry is not applicable to the selected object snap mode). The point returned by `osnap`  depends on the current 3D view, the AutoCAD entity around *pt*, and the setting of the AutoCAD APERTURE system variable.

## Remarks

Starting with AutoCAD 2016-based products, the `qui`  mode is no longer valid. Using the `qui`  mode results in a value of `nil`  to be returned, even if other modes are specified.

## Examples

```lisp
(setq pt1 (getpoint))

(11.8637 3.28269 0.0)

(setq pt2 (osnap pt1 "_end,_int"))

(12.1424 3.42181 0.0)
```
