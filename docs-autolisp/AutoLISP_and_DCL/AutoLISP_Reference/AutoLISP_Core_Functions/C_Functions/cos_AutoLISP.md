---
title: cos (AutoLISP)
guid: "GUID-5F39D690-A986-498D-929A-56BC891E08CD"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5F39D690-A986-498D-929A-56BC891E08CD.htm"
generated: "2025-11-28T19:06:26.114661Z"
description: Returns the cosine of an angle expressed in radians
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

# cos (AutoLISP)

> Returns the cosine of an angle expressed in radians

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5F39D690-A986-498D-929A-56BC891E08CD.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5F39D690-A986-498D-929A-56BC891E08CD.htm)
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
(cos
ang
)
```

- ***ang*:** **Type:**  Integer or Real  An angle, in radians.

## Return Values

**Type:**  Real

The cosine of *ang*, in radians.

## Examples

```lisp
(cos 0.0)

1.0

(cos pi)

-1.0
```
