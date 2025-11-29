---
title: car (AutoLISP)
guid: "GUID-2DD1AF33-415C-4C1A-9631-DA958134C53A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2DD1AF33-415C-4C1A-9631-DA958134C53A.htm"
generated: "2025-11-28T19:06:25.302587Z"
description: Returns the first element of a list
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

# car (AutoLISP)

> Returns the first element of a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2DD1AF33-415C-4C1A-9631-DA958134C53A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2DD1AF33-415C-4C1A-9631-DA958134C53A.htm)
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
(car
list
)
```

- ***list*:** **Type:**  List  A list with one or more elements.

## Return Values

**Type:**  Integer, Real, String, List, T or nil

The first element in *list*; otherwise `nil`, if the list is empty.

## Remarks

In AutoLISP, `car`  is frequently used to obtain the *X*  coordinate of a 2D or 3D point (the first element of a list of two or three reals).

## Examples

```lisp
(car '(a b c))

A

(car '((a b) c))

(A B)

(car '())

nil
```
