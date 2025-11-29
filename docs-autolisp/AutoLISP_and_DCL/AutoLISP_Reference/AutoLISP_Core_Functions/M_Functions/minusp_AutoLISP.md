---
title: minusp (AutoLISP)
guid: "GUID-44E8AFFA-B322-4B26-B436-D83FDAB5CCDB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44E8AFFA-B322-4B26-B436-D83FDAB5CCDB.htm"
generated: "2025-11-28T19:06:37.329442Z"
description: Verifies that a number is negative
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

# minusp (AutoLISP)

> Verifies that a number is negative

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44E8AFFA-B322-4B26-B436-D83FDAB5CCDB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44E8AFFA-B322-4B26-B436-D83FDAB5CCDB.htm)
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
(minusp
num
)
```

- ***num*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  T or nil

`T`  if *number*  is negative; otherwise `nil`.

## Examples

```lisp
(minusp -1)

T

(minusp -4.293)

T

(minusp 830.2)

nil
```
