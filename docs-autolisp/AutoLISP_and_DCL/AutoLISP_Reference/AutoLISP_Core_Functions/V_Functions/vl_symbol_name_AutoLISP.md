---
title: "vl-symbol-name (AutoLISP)"
guid: "GUID-6F5B5B70-3C2D-4301-84BE-FDEEF0C32B30"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6F5B5B70-3C2D-4301-84BE-FDEEF0C32B30.htm"
generated: "2025-11-28T19:06:51.162825Z"
description: Returns a string containing the name of a symbol
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

# vl-symbol-name (AutoLISP)

> Returns a string containing the name of a symbol

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6F5B5B70-3C2D-4301-84BE-FDEEF0C32B30.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6F5B5B70-3C2D-4301-84BE-FDEEF0C32B30.htm)
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
(vl-symbol-name
symbol
)
```

- ***symbol*:** **Type:**  Symbol  Any LISP symbol.

## Return Values

**Type:**  String or error

A string containing the name of the supplied symbol argument, in uppercase.

## Examples

```lisp
(vl-symbol-name 'S::STARTUP)

"S::STARTUP"

(progn (setq sym 'my-var) (vl-symbol-name sym))

"MY-VAR"

(vl-symbol-name 1)

; *** ERROR: bad argument type: symbolp 1
```
