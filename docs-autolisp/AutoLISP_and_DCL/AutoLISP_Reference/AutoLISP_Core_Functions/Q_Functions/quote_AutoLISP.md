---
title: quote (AutoLISP)
guid: "GUID-18F7E287-CB2F-4150-9A07-CE23C3F9E604"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-18F7E287-CB2F-4150-9A07-CE23C3F9E604.htm"
generated: "2025-11-28T19:06:39.293468Z"
description: Returns an expression without evaluating it
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

# quote (AutoLISP)

> Returns an expression without evaluating it

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-18F7E287-CB2F-4150-9A07-CE23C3F9E604.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-18F7E287-CB2F-4150-9A07-CE23C3F9E604.htm)
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
(quote
expr
)
```

- ***expr*:** **Type:**  Integer, Real, String, List, Symbol, File, Subroutine, Ename (entity name), T, or nil  An AutoLISP expression.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, File, Subroutine, Ename (entity name), T, or nil

The *expr*  argument.

## Examples

```lisp
(quote a)

A
```

The previous expression can also be written as `'a`. For example:

```lisp
!'a

A

(quote (a b))

(A B)
```
