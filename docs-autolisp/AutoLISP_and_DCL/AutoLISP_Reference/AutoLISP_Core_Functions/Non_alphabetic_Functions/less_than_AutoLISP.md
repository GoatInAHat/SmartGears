---
title: < (less than) (AutoLISP)
guid: "GUID-67C8900F-0491-4D91-AA05-9136639E7424"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-67C8900F-0491-4D91-AA05-9136639E7424.htm"
generated: "2025-11-28T19:06:19.902667Z"
description: Returns T if each argument is numerically less than the argument to its right; otherwise nil
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

# < (less than) (AutoLISP)

> Returns T if each argument is numerically less than the argument to its right; otherwise nil

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-67C8900F-0491-4D91-AA05-9136639E7424.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-67C8900F-0491-4D91-AA05-9136639E7424.htm)
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
(<
numstr [numstr ...]
)
```

- ***numstr*:** **Type:**  Integer, Real, or String  A number or string.

## Return Values

**Type:**  T or nil

`T`, if each argument is numerically less than the argument to its right; otherwise returns `nil`  . If only one argument is supplied, `T`  is returned.

## Examples

```lisp
(< 10 20)

T

(< "b" "c")

T

(< 357 33.2)

nil

(< 2 3 88)

T

(< 2 3 4 4)

nil
```
