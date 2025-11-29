---
title: "vl-consp (AutoLISP)"
guid: "GUID-36CFF983-0020-4300-845B-BA9768F1B4E8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36CFF983-0020-4300-845B-BA9768F1B4E8.htm"
generated: "2025-11-28T19:06:45.867266Z"
description: Determines whether or not a list is nil
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

# vl-consp (AutoLISP)

> Determines whether or not a list is nil

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36CFF983-0020-4300-845B-BA9768F1B4E8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36CFF983-0020-4300-845B-BA9768F1B4E8.htm)
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
(vl-consp
list-variable
)
```

- ***list-variable*:** **Type:**  Integer, Real, String, List, Subroutine, Ename (entity name), T, nil, or catch-all-apply-error  A list.

## Return Values

**Type:**  T or nil

`T`, if *list-variable*  is a list and is not `nil`; otherwise `nil`.

## Remarks

The `vl-consp`  function determines whether a variable contains a valid list definition.

## Examples

```lisp
(vl-consp nil)

nil

(vl-consp t)

nil

(vl-consp (cons 0 "LINE"))

T
```
