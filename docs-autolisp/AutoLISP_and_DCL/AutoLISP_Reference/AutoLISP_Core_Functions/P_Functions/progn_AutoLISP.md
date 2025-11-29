---
title: progn (AutoLISP)
guid: "GUID-6B2DD269-858D-4C01-ABDB-765DD08284FE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6B2DD269-858D-4C01-ABDB-765DD08284FE.htm"
generated: "2025-11-28T19:06:38.958100Z"
description: Evaluates each expression sequentially and returns the value of the last expression
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

# progn (AutoLISP)

> Evaluates each expression sequentially and returns the value of the last expression

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6B2DD269-858D-4C01-ABDB-765DD08284FE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6B2DD269-858D-4C01-ABDB-765DD08284FE.htm)
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
(progn
[expr ...]
)
```

- ***expr*:** **Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil  One or more AutoLISP expressions.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil

The result of the last evaluated expression.

## Remarks

You can use `progn`  to evaluate several expressions where only one expression is expected.

## Examples

The `if`  function normally evaluates one *then*  expression if the test expression evaluates to anything but `nil`. The following example uses `progn`  to evaluate two expressions following `if`:

```lisp
(if (= a b)
  (progn
    (princ "\nA = B ")
    (setq a (+ a 10) b (- b 10))
  )
)
```
