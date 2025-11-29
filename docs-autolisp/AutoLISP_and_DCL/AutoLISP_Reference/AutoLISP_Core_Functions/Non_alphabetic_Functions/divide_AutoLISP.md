---
title: / (divide) (AutoLISP)
guid: "GUID-E9E70DA9-6048-470D-B899-239B097671D2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E9E70DA9-6048-470D-B899-239B097671D2.htm"
generated: "2025-11-28T19:06:19.654199Z"
description: Divides the first number by the product of the remaining numbers and returns the quotient
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

# / (divide) (AutoLISP)

> Divides the first number by the product of the remaining numbers and returns the quotient

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E9E70DA9-6048-470D-B899-239B097671D2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E9E70DA9-6048-470D-B899-239B097671D2.htm)
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
(/
[number number ...]
)
```

- *****number***:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

The result of the division. If you supply more than two *number*  arguments, this function divides the first number by the product of the second through the last numbers, and returns the final quotient. If you supply one *number*  argument, this function returns the result of dividing it by one; it returns the number. Supplying no arguments returns 0.

## Examples

```lisp
(/ 100 2)

50

(/ 100 2.0)

50.0

(/ 100 20.0 2)

2.5

(/ 100 20 2)

2

(/ 4)

4
```
