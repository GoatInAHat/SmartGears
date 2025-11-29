---
title: null (AutoLISP)
guid: "GUID-2CA2E5F1-297F-4ECA-9500-5FFCD4877126"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2CA2E5F1-297F-4ECA-9500-5FFCD4877126.htm"
generated: "2025-11-28T19:06:37.941861Z"
description: Verifies that an item is bound to nil
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

# null (AutoLISP)

> Verifies that an item is bound to nil

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2CA2E5F1-297F-4ECA-9500-5FFCD4877126.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2CA2E5F1-297F-4ECA-9500-5FFCD4877126.htm)
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
(null
item
)
```

- ***item*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  An AutoLISP expression.

## Return Values

**Type:**  T or nil

`T`  if *item*  evaluates to `nil`; otherwise `nil`.

## Examples

```lisp
(setq a 123 b "string" c nil)

nil

(null a)

nil

(null b)

nil

(null c)

T

(null '())

T
```
