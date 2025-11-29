---
title: * (multiply) (AutoLISP)
guid: "GUID-F7AABF2A-D2D2-4B8A-913C-98082ECB5B8D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F7AABF2A-D2D2-4B8A-913C-98082ECB5B8D.htm"
generated: "2025-11-28T19:06:20.089331Z"
description: Returns the product of all numbers
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

# * (multiply) (AutoLISP)

> Returns the product of all numbers

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F7AABF2A-D2D2-4B8A-913C-98082ECB5B8D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F7AABF2A-D2D2-4B8A-913C-98082ECB5B8D.htm)
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
(*
[number number ...]
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

The result of the multiplication. If you supply only one *number*  argument, this function returns the result of multiplying it by one; it returns the number. Supplying no arguments returns 0.

## Examples

```lisp
(* 2 3)

6

(* 2 3.0)

6.0

(* 2 3 4.0)

24.0

(* 3 -4.5)

-13.5

(* 3)

3
```
