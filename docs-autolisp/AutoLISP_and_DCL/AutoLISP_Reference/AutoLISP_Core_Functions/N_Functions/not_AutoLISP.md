---
title: not (AutoLISP)
guid: "GUID-991AD6A0-61AA-45C9-8C27-CADF9F36BE71"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-991AD6A0-61AA-45C9-8C27-CADF9F36BE71.htm"
generated: "2025-11-28T19:06:37.754192Z"
description: Verifies that an item evaluates to nil
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

# not (AutoLISP)

> Verifies that an item evaluates to nil

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-991AD6A0-61AA-45C9-8C27-CADF9F36BE71.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-991AD6A0-61AA-45C9-8C27-CADF9F36BE71.htm)
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
(not
item
)
```

- ***item*:** **Type:**  Integer, Real, String, List, Subroutine, Ename (entity name), T, or nil  An AutoLISP expression.

## Return Values

**Type:**  T or nil

`T`  if *item*  evaluates to `nil`; otherwise `nil`.

## Remarks

Typically, the `null`  function is used for lists, and `not`  is used for other data types along with some types of control functions.

## Examples

```lisp
(setq a 123 b "string" c nil)

nil

(not a)

nil

(not b)

nil

(not c)

T

(not '())

T
```
