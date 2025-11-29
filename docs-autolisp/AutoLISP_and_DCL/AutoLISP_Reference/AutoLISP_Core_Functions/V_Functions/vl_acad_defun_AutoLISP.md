---
title: "vl-acad-defun (AutoLISP)"
guid: "GUID-D15CB87F-5B69-4BC3-B8BB-E45D57602B27"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D15CB87F-5B69-4BC3-B8BB-E45D57602B27.htm"
generated: "2025-11-28T19:06:44.840630Z"
description: Defines an AutoLISP function symbol as an external subroutine
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

# vl-acad-defun (AutoLISP)

> Defines an AutoLISP function symbol as an external subroutine

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D15CB87F-5B69-4BC3-B8BB-E45D57602B27.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D15CB87F-5B69-4BC3-B8BB-E45D57602B27.htm)
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
(vl-acad-defun
'symbol
)
```

- **symbol:** **Type:**  Subroutine or Symbol  A symbol identifying a function.

## Return Values

**Type:**  Integer

A numeric value.

## Remarks

If a function does not have the `c:`  prefix, and you want to be able to invoke this function from an external ObjectARX application, you can use `vl-acad-defun`  to make the function accessible.

## Examples

None
