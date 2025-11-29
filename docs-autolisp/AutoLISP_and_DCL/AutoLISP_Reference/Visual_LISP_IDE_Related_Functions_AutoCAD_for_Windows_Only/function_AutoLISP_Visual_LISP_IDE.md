---
title: function (AutoLISP/Visual LISP IDE)
guid: "GUID-CF7E5870-561F-42DB-B134-CCD41EF93A25"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CF7E5870-561F-42DB-B134-CCD41EF93A25.htm"
generated: "2025-11-28T19:06:53.967608Z"
description: "Tells the Visual LISP compiler to link and optimize an argument as if it were a built-in function"
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

# function (AutoLISP/Visual LISP IDE)

> Tells the Visual LISP compiler to link and optimize an argument as if it were a built-in function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CF7E5870-561F-42DB-B134-CCD41EF93A25.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CF7E5870-561F-42DB-B134-CCD41EF93A25.htm)
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
(function
symbol | lambda-expr
)
```

- ***symbol*:** **Type:**  Symbol  A symbol naming a function.
- ***lambda-expr*:** **Type:**  Subroutine or List  An expression of the following form:  `(LAMBDA *arguments {S-expression}**)`

## Return Values

The result of the evaluated expression.

## Remarks

The `function`  function is identical to the `quote`  function, except it tells the Visual LISP compiler to link and optimize the argument as if it were a built-in function or `defun`.

Compiled `lambda`  expressions that are quoted by `function`  will contain debugging information when loaded into the Visual LISP IDE.

## Examples

The Visual LISP compiler cannot optimize the quoted `lambda`  expression in the following code:

```lisp
(mapcar
  '(lambda (x) (* x x))
       '(1 2 3))
```

After adding the `function`  function to the expression, the compiler can optimize the `lambda`  expression. For example:

```lisp
(mapcar
   (function (lambda (x) (* x x)))
      '(1 2 3))
```
