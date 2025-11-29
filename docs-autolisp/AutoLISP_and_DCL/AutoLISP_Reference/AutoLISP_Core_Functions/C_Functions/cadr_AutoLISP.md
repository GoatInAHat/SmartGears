---
title: cadr (AutoLISP)
guid: "GUID-F8E9E4F0-218D-4587-9D7E-922BE57C9F9B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F8E9E4F0-218D-4587-9D7E-922BE57C9F9B.htm"
generated: "2025-11-28T19:06:25.211520Z"
description: Returns the second element of a list
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

# cadr (AutoLISP)

> Returns the second element of a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F8E9E4F0-218D-4587-9D7E-922BE57C9F9B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F8E9E4F0-218D-4587-9D7E-922BE57C9F9B.htm)
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
(cadr
list
)
```

- ***list*:** **Type:**  List  A list with two or more elements.

## Return Values

**Type:**  Integer, Real, String, List, T or nil

The second element in *list*; otherwise `nil`, if the list is empty or contains only one element.

## Remarks

In AutoLISP, `cadr`  is frequently used to obtain the *Y*  coordinate of a 2D or 3D point (the second element of a list of two or three reals).

## Examples

```lisp
(setq pt2 '(5.25 1.0))

(5.25 1.0)

(cadr pt2)

1.0

(cadr '(4.0))

nil

(cadr '(5.25 1.0 3.0))

1.0
```
