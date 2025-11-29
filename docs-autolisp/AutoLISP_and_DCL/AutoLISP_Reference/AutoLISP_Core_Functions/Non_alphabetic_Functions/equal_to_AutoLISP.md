---
title: = (equal to) (AutoLISP)
guid: "GUID-3F1959A8-AEC4-47C5-8E9E-80364BCA93B2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F1959A8-AEC4-47C5-8E9E-80364BCA93B2.htm"
generated: "2025-11-28T19:06:20.736948Z"
description: Compares arguments for numerical equality
topic_type: concept
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
tags:
  - operators = (equal to)
  - equal to operator
  - = (equal to)
  - equality between expressions
  - numbers checking equality of
  - mathematical functions equality checking
---

# = (equal to) (AutoLISP)

> Compares arguments for numerical equality

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F1959A8-AEC4-47C5-8E9E-80364BCA93B2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F1959A8-AEC4-47C5-8E9E-80364BCA93B2.htm)
- Topic Type: concept
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023
- Keywords: operators = (equal to), equal to operator, = (equal to), equality between expressions, numbers checking equality of, mathematical functions equality checking

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(=
numstr [numstr ...]
)
```

- ***numstr*:** **Type:**  Integer, Real, or String  A number or string.

## Return Values

**Type:**  T or nil

`T`, if all arguments are numerically equal; otherwise `nil`. If only one argument is supplied, `T`  is returned.

## Examples

```lisp
(= 4 4.0)

T

(= 20 388)

nil

(= 2.4 2.4 2.4)

T

(= 499 499 500)

nil

(= "me" "me")

T

(= "me" "you")

nil
```
