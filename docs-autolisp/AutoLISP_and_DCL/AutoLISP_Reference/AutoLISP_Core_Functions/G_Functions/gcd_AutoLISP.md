---
title: gcd (AutoLISP)
guid: "GUID-3F748964-B6D7-4136-92D5-81A7DD02ED4A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F748964-B6D7-4136-92D5-81A7DD02ED4A.htm"
generated: "2025-11-28T19:06:30.285908Z"
description: Returns the greatest common denominator of two integers
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

# gcd (AutoLISP)

> Returns the greatest common denominator of two integers

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F748964-B6D7-4136-92D5-81A7DD02ED4A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3F748964-B6D7-4136-92D5-81A7DD02ED4A.htm)
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
(gcd
int1 int2
)
```

- ***int1*:** **Type:**  Integer  An integer; must be greater than 0.
- ***int2*:** **Type:**  Integer  An integer; must be greater than 0.

## Return Values

**Type:**  Integer

An integer representing the greatest common denominator between *int1*  and *int2*.

## Examples

```lisp
(gcd 81 57)

3

(gcd 12 20)

4
```
