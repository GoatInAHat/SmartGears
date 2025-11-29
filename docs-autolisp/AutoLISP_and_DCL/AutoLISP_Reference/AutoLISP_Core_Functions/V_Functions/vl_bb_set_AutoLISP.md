---
title: "vl-bb-set (AutoLISP)"
guid: "GUID-2B1484FC-BBD1-408B-A8A0-DB931363630F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2B1484FC-BBD1-408B-A8A0-DB931363630F.htm"
generated: "2025-11-28T19:06:45.237518Z"
description: Sets a variable in the blackboard namespace
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

# vl-bb-set (AutoLISP)

> Sets a variable in the blackboard namespace

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2B1484FC-BBD1-408B-A8A0-DB931363630F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2B1484FC-BBD1-408B-A8A0-DB931363630F.htm)
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
(vl-bb-set
'symbol value
)
```

- ***'symbol*:** **Type:**  Symbol  A symbol naming the variable to be set.
- ***value*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  Any value, except a function.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, or nil

The *value*  you assigned to *symbol*.

## Examples

```lisp
(vl-bb-set 'foobar "Root toot toot")

"Root toot toot"

(vl-bb-ref 'foobar)

"Root toot toot"
```
