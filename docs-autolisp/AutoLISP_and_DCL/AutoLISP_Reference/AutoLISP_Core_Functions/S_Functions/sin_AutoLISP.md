---
title: sin (AutoLISP)
guid: "GUID-83677851-6FA5-496E-A93C-AEA55ABCB527"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-83677851-6FA5-496E-A93C-AEA55ABCB527.htm"
generated: "2025-11-28T19:06:41.279578Z"
description: Returns the sine of an angle as a real number expressed in radians
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

# sin (AutoLISP)

> Returns the sine of an angle as a real number expressed in radians

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-83677851-6FA5-496E-A93C-AEA55ABCB527.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-83677851-6FA5-496E-A93C-AEA55ABCB527.htm)
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
(sin
ang
)
```

- ***ang*:** **Type:**  Integer or Real  An angle, in radians.

## Return Values

**Type:**  Real

A number representing the sine of *ang*, in radians.

## Examples

```lisp
(sin 1.0)

0.841471

(sin 0.0)

0.0
```
