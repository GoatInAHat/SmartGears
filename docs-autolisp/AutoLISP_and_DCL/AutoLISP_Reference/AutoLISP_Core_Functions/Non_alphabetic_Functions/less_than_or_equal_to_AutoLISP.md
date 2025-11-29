---
title: <= (less than or equal to) (AutoLISP)
guid: "GUID-66B7B146-E279-4C22-9C0A-1F312166C15B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-66B7B146-E279-4C22-9C0A-1F312166C15B.htm"
generated: "2025-11-28T19:06:20.001540Z"
description: Returns T if each argument is numerically less than or equal to the argument to its right; otherwise returns nil
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

# <= (less than or equal to) (AutoLISP)

> Returns T if each argument is numerically less than or equal to the argument to its right; otherwise returns nil

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-66B7B146-E279-4C22-9C0A-1F312166C15B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-66B7B146-E279-4C22-9C0A-1F312166C15B.htm)
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
(<=
numstr [numstr ...]
)
```

- ***numstr*:** **Type:**  Integer, Real, or String  A number or string.

## Return Values

**Type:**  T or nil

`T`, if each argument is numerically less than or equal to the argument to its right; otherwise returns `nil`. If only one argument is supplied, `T`  is returned.

## Examples

```lisp
(<= 10 20)

T

(<= "b" "b")

T

(<= 357 33.2)

nil

(<= 2 9 9)

T

(<= 2 9 4 5)

nil
```
