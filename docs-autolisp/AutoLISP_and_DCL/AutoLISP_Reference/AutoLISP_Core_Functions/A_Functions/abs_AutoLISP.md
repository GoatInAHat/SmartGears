---
title: abs (AutoLISP)
guid: "GUID-3AF854D7-9B75-4A4B-ABA4-E47DB90FB2F6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3AF854D7-9B75-4A4B-ABA4-E47DB90FB2F6.htm"
generated: "2025-11-28T19:06:21.057511Z"
description: Returns the absolute value of a number
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

# abs (AutoLISP)

> Returns the absolute value of a number

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3AF854D7-9B75-4A4B-ABA4-E47DB90FB2F6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3AF854D7-9B75-4A4B-ABA4-E47DB90FB2F6.htm)
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
(abs
number
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

The absolute value of the *number*  argument.

## Examples

```lisp
(abs 100)

100

(abs -100)

100

(abs -99.25)

99.25
```
