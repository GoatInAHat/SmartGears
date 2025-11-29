---
title: + (add) (AutoLISP)
guid: "GUID-FCD1F559-F9EA-49F3-AE53-8FF7D90B3B4C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCD1F559-F9EA-49F3-AE53-8FF7D90B3B4C.htm"
generated: "2025-11-28T19:06:19.565858Z"
description: Returns the sum of all numbers
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

# + (add) (AutoLISP)

> Returns the sum of all numbers

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCD1F559-F9EA-49F3-AE53-8FF7D90B3B4C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCD1F559-F9EA-49F3-AE53-8FF7D90B3B4C.htm)
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
(+
[number number ...]
)
```

- *****number***:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

The result of the addition. If you supply only one *number*  argument, this function returns the result of adding it to zero. If you supply no arguments, the function returns 0.

## Examples

```lisp
(+ 1 2)

3

(+ 1 2 3 4.5)

10.5

(+ 1 2 3 4.0)

10.0
```
