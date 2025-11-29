---
title: lambda (AutoLISP)
guid: "GUID-3B8BB020-1E1A-4FA3-B7B3-B5B20BA04CD9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3B8BB020-1E1A-4FA3-B7B3-B5B20BA04CD9.htm"
generated: "2025-11-28T19:06:33.798781Z"
description: Defines an anonymous function
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

# lambda (AutoLISP)

> Defines an anonymous function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3B8BB020-1E1A-4FA3-B7B3-B5B20BA04CD9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3B8BB020-1E1A-4FA3-B7B3-B5B20BA04CD9.htm)
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
(lambda
arguments expr ...
)
```

- ***arguments*:** **Type:**  List  Arguments passed to an expression.
- ***expr*:** **Type:**  List  An AutoLISP expression.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, Ename (entity name), T, or nil

Value of the last *expr*.

## Remarks

Use the `lambda`  function when the overhead of defining a new function is not justified. It also makes your intention more apparent by laying out the function at the spot where it is to be used. This function returns the value of its last *expr*, and is often used in conjunction with `apply`  and/or `mapcar`  to perform a function on a list.

## Examples

The following examples demonstrate the `lambda`  function:

```lisp
(apply '(lambda (x y z)
          (* x (- y z))
        )
        '(5 20 14)
)

30

(setq counter 0)
(mapcar '(lambda (x)
          (setq counter (1+ counter))
          (* x 5)
        )
        '(2 4 -6 10.2)
)

0
(10 20 -30 51.0)
```
