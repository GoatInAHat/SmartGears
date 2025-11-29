---
title: >= (greater than or equal to) (AutoLISP)
guid: "GUID-586BD9C1-441C-47DE-B6E2-1445518CABBA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-586BD9C1-441C-47DE-B6E2-1445518CABBA.htm"
generated: "2025-11-28T19:06:19.823902Z"
description: Returns T if each argument is numerically greater than or equal to the argument to its right; otherwise returns nil
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

# >= (greater than or equal to) (AutoLISP)

> Returns T if each argument is numerically greater than or equal to the argument to its right; otherwise returns nil

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-586BD9C1-441C-47DE-B6E2-1445518CABBA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-586BD9C1-441C-47DE-B6E2-1445518CABBA.htm)
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
(>=
numstr [numstr ...]
)
```

- ***numstr*:** **Type:**  Integer, Real, or String  A number or string.

## Return Values

**Type:**  T or nil

`T`, if each argument is numerically greater than or equal to the argument to its right; otherwise `nil`. If only one argument is supplied, `T`  is returned.

## Examples

```lisp
(>= 120 17)

T

(>= "c" "C")

T

(>= 3.5 1792)

nil

(>= 77 4 2)

T

(>= 77 4 9)

nil
```
