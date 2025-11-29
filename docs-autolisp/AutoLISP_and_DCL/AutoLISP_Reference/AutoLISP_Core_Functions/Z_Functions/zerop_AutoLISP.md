---
title: zerop (AutoLISP)
guid: "GUID-3D509EE1-A809-4B78-915C-28ECF483BB72"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3D509EE1-A809-4B78-915C-28ECF483BB72.htm"
generated: "2025-11-28T19:06:52.939008Z"
description: Verifies that a number evaluates to zero
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

# zerop (AutoLISP)

> Verifies that a number evaluates to zero

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3D509EE1-A809-4B78-915C-28ECF483BB72.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3D509EE1-A809-4B78-915C-28ECF483BB72.htm)
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
(zerop
number
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  T or nil

`T`  if *number*  evaluates to zero; otherwise `nil`.

## Examples

```lisp
(zerop 0)

T

(zerop 0.0)

T

(zerop 0.0001)

nil
```
