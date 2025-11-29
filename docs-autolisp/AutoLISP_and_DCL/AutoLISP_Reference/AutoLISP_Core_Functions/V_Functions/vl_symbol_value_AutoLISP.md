---
title: "vl-symbol-value (AutoLISP)"
guid: "GUID-5DAAABCF-7210-4A90-809D-6C27CCEF64F1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5DAAABCF-7210-4A90-809D-6C27CCEF64F1.htm"
generated: "2025-11-28T19:06:51.248463Z"
description: Returns the current value bound to a symbol
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

# vl-symbol-value (AutoLISP)

> Returns the current value bound to a symbol

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5DAAABCF-7210-4A90-809D-6C27CCEF64F1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5DAAABCF-7210-4A90-809D-6C27CCEF64F1.htm)
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
(vl-symbol-value
symbol
)
```

- ***symbol*:** **Type:**  Symbol  Any LISP symbol.

## Return Values

**Type:**  Integer, Real, String, List, File, Ename (entity name), T, or nil

The value of *symbol*, after evaluation.

## Remarks

This function is equivalent to the `eval`  function, but does not call the LISP evaluator.

## Examples

```lisp
(vl-symbol-value 't)

T

(vl-symbol-value 'PI)

3.14159

(progn (setq sym 'PAUSE) (vl-symbol-value sym))

"\\"
```
