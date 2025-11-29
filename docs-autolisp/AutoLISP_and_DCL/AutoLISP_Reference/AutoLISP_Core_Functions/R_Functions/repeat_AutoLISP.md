---
title: repeat (AutoLISP)
guid: "GUID-413F72B4-BA37-4E5E-9D51-A0091130A317"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-413F72B4-BA37-4E5E-9D51-A0091130A317.htm"
generated: "2025-11-28T19:06:39.911781Z"
description: Evaluates each expression a specified number of times, and returns the value of the last expression
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

# repeat (AutoLISP)

> Evaluates each expression a specified number of times, and returns the value of the last expression

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-413F72B4-BA37-4E5E-9D51-A0091130A317.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-413F72B4-BA37-4E5E-9D51-A0091130A317.htm)
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
(repeat
int [expr ...]
)
```

- ***int*:** **Type:**  Integer  A numeric value. Must be a positive number.
- ***expr*:** **Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil  One or more atoms or expressions.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil

The value of the last expression or atom evaluated. If *expr*  is not supplied, `repeat`  returns `nil`.

## Examples

```lisp
(setq a 10 b 100)

100

(repeat 4 (setq a (+ a 10)) (setq b (+ b 100)))

500
```

After evaluation, `a`  is 50, `b`  is 500, and `repeat`  returns 500.

If strings are supplied as arguments, `repeat`  returns the last string:

```lisp
(repeat 100 "Me" "You")

"You"
```
