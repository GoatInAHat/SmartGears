---
title: eval (AutoLISP)
guid: "GUID-D9B3E6CC-A982-4040-AE6E-6FD63D6C54D0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D9B3E6CC-A982-4040-AE6E-6FD63D6C54D0.htm"
generated: "2025-11-28T19:06:28.941439Z"
description: Returns the result of evaluating an AutoLISP expression
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

# eval (AutoLISP)

> Returns the result of evaluating an AutoLISP expression

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D9B3E6CC-A982-4040-AE6E-6FD63D6C54D0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D9B3E6CC-A982-4040-AE6E-6FD63D6C54D0.htm)
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
(eval
expr
)
```

- ***expr*:** **Type:**  Integer, Real, String, List, Symbol, Ename (entity name), T, or nil  The expression to be evaluated.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, Ename (entity name), T, or nil

The result of the expression, after evaluation.

## Examples

First, set some variables:

```lisp
(setq a 123)

123

(setq b 'a)

A
```

Now evaluate some expressions:

```lisp
(eval 4.0)

4.0

(eval (abs -10))

10

(eval a)

123

(eval b)

123
```
