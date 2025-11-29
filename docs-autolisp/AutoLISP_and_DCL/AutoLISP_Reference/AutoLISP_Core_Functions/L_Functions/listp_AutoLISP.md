---
title: listp (AutoLISP)
guid: "GUID-8755F083-D3BA-45F0-BBAF-D60D3A73240C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8755F083-D3BA-45F0-BBAF-D60D3A73240C.htm"
generated: "2025-11-28T19:06:35.990666Z"
description: Verifies that an item is a list
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

# listp (AutoLISP)

> Verifies that an item is a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8755F083-D3BA-45F0-BBAF-D60D3A73240C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8755F083-D3BA-45F0-BBAF-D60D3A73240C.htm)
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
(listp
item
)
```

- ***item*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  Any atom, list, or expression.

## Return Values

**Type:**  T or nil

`T`  if *item*  is a list; otherwise `nil`. Because `nil`  is both an atom and a list, the `listp`  function returns `T`  when passed `nil`.

## Examples

```lisp
(listp '(a b c))

T

(listp 'a)

nil

(listp 4.343)

nil

(listp nil)

T

(listp (setq v1 '(1 2 43)))

T
```
