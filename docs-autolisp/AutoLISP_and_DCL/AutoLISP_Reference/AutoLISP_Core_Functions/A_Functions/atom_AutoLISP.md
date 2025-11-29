---
title: atom (AutoLISP)
guid: "GUID-3B74E28D-7C3B-4409-9DFE-0305E9881560"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3B74E28D-7C3B-4409-9DFE-0305E9881560.htm"
generated: "2025-11-28T19:06:24.556944Z"
description: Verifies that an item is an atom
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

# atom (AutoLISP)

> Verifies that an item is an atom

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3B74E28D-7C3B-4409-9DFE-0305E9881560.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3B74E28D-7C3B-4409-9DFE-0305E9881560.htm)
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
(atom
item
)
```

- ***item*:** **Type:**  Symbol  Any AutoLISP element.  Some versions of LISP differ in their interpretation of `atom`, so be careful when converting from non-AutoLISP code.

## Return Values

**Type:**  T or nil

`nil`  if *item*  is a list; otherwise `T`. Anything that is not a list is considered an atom.

## Examples

```lisp
(setq a '(x y z))

(X Y Z)

(setq b 'a)

A

(atom 'a)

T

(atom a)

nil

(atom 'b)

T

(atom b)

T

(atom '(a b c))

nil
```
