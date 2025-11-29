---
title: rem (AutoLISP)
guid: "GUID-4CC43BEB-3215-41AE-839B-2DCE6354241A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CC43BEB-3215-41AE-839B-2DCE6354241A.htm"
generated: "2025-11-28T19:06:39.839537Z"
description: Divides the first number by the second, and returns the remainder
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

# rem (AutoLISP)

> Divides the first number by the second, and returns the remainder

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CC43BEB-3215-41AE-839B-2DCE6354241A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CC43BEB-3215-41AE-839B-2DCE6354241A.htm)
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
(rem
[number number ...]
)
```

- ***number*:** **Type:**  Integer or Real  Any number.

## Return Values

**Type:**  Integer or Real

A number. If any *number*  argument is a real, `rem`  returns a real; otherwise, `rem`  returns an integer. If no arguments are supplied, `rem`  returns 0. If a single *number*  argument is supplied, `rem`  returns *number*.

## Remarks

If you provide more than two numbers, numbers are evaluated from left to right. For example, if you supply three numbers, `rem`  divides the first number by the second, then takes the result and divides it by the third number, returning the remainder of that operation.

## Examples

```lisp
(rem 42 12)

6

(rem 12.0 16)

12.0

(rem 26 7 2)

1
```
