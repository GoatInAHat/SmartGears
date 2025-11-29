---
title: "- (subtract) (AutoLISP)"
guid: "GUID-53A08CC7-131C-4C7C-B1DA-653559D49E4F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-53A08CC7-131C-4C7C-B1DA-653559D49E4F.htm"
generated: "2025-11-28T19:06:20.532872Z"
description: Subtracts the second and following numbers from the first and returns the difference
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

# - (subtract) (AutoLISP)

> Subtracts the second and following numbers from the first and returns the difference

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-53A08CC7-131C-4C7C-B1DA-653559D49E4F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-53A08CC7-131C-4C7C-B1DA-653559D49E4F.htm)
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
(-
[number number ...]
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

The result of the subtraction. If you supply more than two *number*  arguments, this function returns the result of subtracting the sum of the second through the last numbers from the first number. If you supply only one *number*  argument, this function subtracts the number from zero, and returns a negative number. Supplying no arguments returns 0.

## Examples

```lisp
(- 50 40)

10

(- 50 40.0)

10.0

(- 50 40.0 2.5)

7.5

(- 8)

-8
```
