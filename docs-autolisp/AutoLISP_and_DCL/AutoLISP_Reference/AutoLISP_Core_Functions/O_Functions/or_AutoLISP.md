---
title: or (AutoLISP)
guid: "GUID-64ECF4D5-0714-45FD-8E27-284E9D2FC8B2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-64ECF4D5-0714-45FD-8E27-284E9D2FC8B2.htm"
generated: "2025-11-28T19:06:38.235186Z"
description: Returns the logical OR of a list of expressions
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

# or (AutoLISP)

> Returns the logical OR of a list of expressions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-64ECF4D5-0714-45FD-8E27-284E9D2FC8B2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-64ECF4D5-0714-45FD-8E27-284E9D2FC8B2.htm)
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
(or
[expr ...]
)
```

- ***expr*:** **Type:**  Integer, Real, String, List, Subroutine, Ename (entity name), T, or nil  The expressions to be evaluated.

## Remarks

The `or`  function evaluates the expressions from left to right, looking for a non- `nil`  expression.

## Return Values

**Type:**  T or nil

`T`, if a non- `nil`  expression is found; otherwise `nil`, if all of the expressions are `nil`  or no arguments are supplied.

Note that `or`  accepts an atom as an argument and returns `T`  if one is supplied.

## Examples

```lisp
(or nil 45 '())

T

(or nil '())

nil
```
