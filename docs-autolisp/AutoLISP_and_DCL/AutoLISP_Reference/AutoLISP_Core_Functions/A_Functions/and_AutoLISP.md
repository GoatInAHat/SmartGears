---
title: and (AutoLISP)
guid: "GUID-59FC3712-44E9-4D09-A611-5B153A26AE5E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-59FC3712-44E9-4D09-A611-5B153A26AE5E.htm"
generated: "2025-11-28T19:06:22.680164Z"
description: Returns the logical AND of the supplied arguments
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

# and (AutoLISP)

> Returns the logical AND of the supplied arguments

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-59FC3712-44E9-4D09-A611-5B153A26AE5E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-59FC3712-44E9-4D09-A611-5B153A26AE5E.htm)
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
(and
[expr ...]
)
```

- ***expr*:** **Type:**  Symbol, Integer, Real, String, T, or nil  Any expression.

## Return Values

**Type:**  T or nil

`nil`, if any of the expressions evaluate to `nil`; otherwise `T`. If `and`  is issued without arguments, it returns `T`.

## Examples

```lisp
(setq a 103 b nil c "string")

"string"

(and 1.4 a c)

T

(and 1.4 a b c)

nil
```
