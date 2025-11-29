---
title: boundp (AutoLISP)
guid: "GUID-F8491426-8505-4390-825F-CACE15EBB48F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F8491426-8505-4390-825F-CACE15EBB48F.htm"
generated: "2025-11-28T19:06:25.033782Z"
description: Verifies if a value is bound to a symbol
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

# boundp (AutoLISP)

> Verifies if a value is bound to a symbol

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F8491426-8505-4390-825F-CACE15EBB48F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F8491426-8505-4390-825F-CACE15EBB48F.htm)
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
(boundp
sym
)
```

- ***sym*:** **Type:**  Symbol  A symbol.

## Return Values

**Type:**  T or nil

`T`  if *sym*  has a value bound to it. If no value is bound to *sym*, or if it has been bound to `nil`, `boundp`  returns `nil`. If *sym*  is an undefined symbol, it is automatically created and is bound to `nil`.

## Examples

```lisp
(setq a 2 b nil)

nil

(boundp 'a)

T

(boundp 'b)

nil
```

The `atoms-family`  function provides an alternative method of determining the existence of a symbol without automatically creating the symbol.
