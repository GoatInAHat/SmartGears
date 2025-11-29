---
title: caddr (AutoLISP)
guid: "GUID-0E81B9F0-7AAD-4BB4-98EB-378AEBE5C4CF"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0E81B9F0-7AAD-4BB4-98EB-378AEBE5C4CF.htm"
generated: "2025-11-28T19:06:25.127512Z"
description: Returns the third element of a list
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

# caddr (AutoLISP)

> Returns the third element of a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0E81B9F0-7AAD-4BB4-98EB-378AEBE5C4CF.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0E81B9F0-7AAD-4BB4-98EB-378AEBE5C4CF.htm)
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
(caddr
list
)
```

- ***list*:** **Type:**  List  A list with three or more elements.

## Return Values

**Type:**  Integer, Real, String, List, T or nil

The third element in *list*; otherwise `nil`, if the list is empty or contains fewer than three elements.

## Remarks

In AutoLISP, `caddr`  is frequently used to obtain the *Z*  coordinate of a 3D point (the third element of a list of three reals).

## Examples

```lisp
(setq pt3 '(5.25 1.0 3.0))

(5.25 1.0 3.0)

(caddr pt3)

3.0

(caddr '(5.25 1.0))

nil
```
