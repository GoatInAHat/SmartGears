---
title: "vl-bb-ref (AutoLISP)"
guid: "GUID-F4DAAAA4-5A96-4AFF-B8AE-C1F4F91C80C9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F4DAAAA4-5A96-4AFF-B8AE-C1F4F91C80C9.htm"
generated: "2025-11-28T19:06:45.108829Z"
description: Returns the value of a variable from the blackboard namespace
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

# vl-bb-ref (AutoLISP)

> Returns the value of a variable from the blackboard namespace

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F4DAAAA4-5A96-4AFF-B8AE-C1F4F91C80C9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F4DAAAA4-5A96-4AFF-B8AE-C1F4F91C80C9.htm)
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
(vl-bb-ref
'variable
)
```

- ***'variable*:** **Type:**  Symbol  A symbol identifying the variable to be retrieved.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, or nil

The value of the *variable*  named by symbol.

## Examples

Set a variable in the blackboard:

```lisp
(vl-bb-set 'foobar "Root toot toot")

"Root toot toot"
```

Use `vl-bb-ref`  to retrieve the value of `foobar`  from the blackboard:

```lisp
(vl-bb-ref 'foobar)

"Root toot toot"
```
