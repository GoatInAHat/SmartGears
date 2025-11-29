---
title: numberp (AutoLISP)
guid: "GUID-1E5B591C-F60B-43CB-8CD8-E729D72B12FC"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1E5B591C-F60B-43CB-8CD8-E729D72B12FC.htm"
generated: "2025-11-28T19:06:38.066990Z"
description: Verifies that an item is a real number or an integer
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

# numberp (AutoLISP)

> Verifies that an item is a real number or an integer

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1E5B591C-F60B-43CB-8CD8-E729D72B12FC.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1E5B591C-F60B-43CB-8CD8-E729D72B12FC.htm)
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
(numberp
item
)
```

- ***item*:** **Type:**  Integer, Real, String, List, Subroutine, Ename (entity name), T, or nil  An AutoLISP expression.

## Return Values

**Type:**  T or nil

`T`  if *item*  evaluates to a real or an integer; otherwise `nil`.

## Examples

```lisp
(setq a 123 b 'a)

A

(numberp 4)

T

(numberp 3.8348)

T

(numberp "Howdy")

nil

(numberp a)

T

(numberp b)

nil

(numberp (eval b))

T
```
