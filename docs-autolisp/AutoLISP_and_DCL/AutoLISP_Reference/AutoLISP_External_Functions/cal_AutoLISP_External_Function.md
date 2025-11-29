---
title: cal (AutoLISP/External Function)
guid: "GUID-C27D945A-B94A-4EAC-B19A-68C82768A4CB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C27D945A-B94A-4EAC-B19A-68C82768A4CB.htm"
generated: "2025-11-28T19:06:53.439127Z"
description: "Invokes the on-line geometry calculator and returns the value of the evaluated expression"
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

# cal (AutoLISP/External Function)

> Invokes the on-line geometry calculator and returns the value of the evaluated expression

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C27D945A-B94A-4EAC-B19A-68C82768A4CB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C27D945A-B94A-4EAC-B19A-68C82768A4CB.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows and Mac OS only; not available in AutoCAD LT

**Prerequisites:** The GeomCal ObjectARX application must be loaded before the function can be called, `(arxload "geomcal")`.

## Signature

```lisp
(c:cal
expression
)
```

- ***expression*:** **Type:**  String  A quoted string.  See the AutoCAD CAL command for a description of allowable expressions.

## Return Values

**Type:**  Integer, Real, List, or nil

The result of the expression.

## Examples

The following example uses `cal`  in an AutoLISP expression with the `trans`  function:

```lisp
(trans (c:cal "[1,2,3]+MID") 1 2)

>> Select entity for MID snap:
(20.0465 16.0369 3.0)
```
