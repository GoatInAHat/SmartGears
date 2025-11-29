---
title: exp (AutoLISP)
guid: "GUID-FD0C918B-A162-4939-9F4E-FFE8863C3FC8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FD0C918B-A162-4939-9F4E-FFE8863C3FC8.htm"
generated: "2025-11-28T19:06:29.250136Z"
description: Returns the constant e (a real number) raised to a specified power (the natural antilog)
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

# exp (AutoLISP)

> Returns the constant e (a real number) raised to a specified power (the natural antilog)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FD0C918B-A162-4939-9F4E-FFE8863C3FC8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FD0C918B-A162-4939-9F4E-FFE8863C3FC8.htm)
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
(exp
num
)
```

- ***num*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Real

A numeric value, raised to its natural antilogarithm.

## Examples

```lisp
(exp 1.0)

2.71828

(exp 2.2)

9.02501

(exp -0.4)

0.67032
```
