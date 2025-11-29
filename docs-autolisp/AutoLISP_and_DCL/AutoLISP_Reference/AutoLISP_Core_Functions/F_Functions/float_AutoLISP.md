---
title: float (AutoLISP)
guid: "GUID-93808F66-808C-49F7-9303-F12C55296080"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-93808F66-808C-49F7-9303-F12C55296080.htm"
generated: "2025-11-28T19:06:29.734781Z"
description: Returns the conversion of a number into a real number
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

# float (AutoLISP)

> Returns the conversion of a number into a real number

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-93808F66-808C-49F7-9303-F12C55296080.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-93808F66-808C-49F7-9303-F12C55296080.htm)
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
(float
number
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Real

The real number derived from *number*.

## Examples

```lisp
(float 3)

3.0

(float 3.75)

3.75
```
