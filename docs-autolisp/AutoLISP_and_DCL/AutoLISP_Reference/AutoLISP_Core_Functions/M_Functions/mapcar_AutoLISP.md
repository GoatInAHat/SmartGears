---
title: mapcar (AutoLISP)
guid: "GUID-8802AE73-1A05-457E-8A51-09677C23E26E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8802AE73-1A05-457E-8A51-09677C23E26E.htm"
generated: "2025-11-28T19:06:36.542710Z"
description: Returns a list that is the result of executing a function with a list (or lists) supplied as arguments to the function
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

# mapcar (AutoLISP)

> Returns a list that is the result of executing a function with a list (or lists) supplied as arguments to the function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8802AE73-1A05-457E-8A51-09677C23E26E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8802AE73-1A05-457E-8A51-09677C23E26E.htm)
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
(mapcar
function list1... listn
)
```

- ***function*:** **Type:**  Subroutine  A function.
- ***list1... listn*:** **Type:**  List  One or more lists. The number of lists must match the number of arguments required by *function*.

## Return Values

**Type:**  List

A list.

## Examples

```lisp
(setq a 10 b 20 c 30)

30

(mapcar '1+ (list a b c))

(11 21 31)
```

This is equivalent to the following series of expressions, except that `mapcar`  returns a list of the results:

```lisp
(1+ a)
(1+ b)
(1+ c)
```

The `lambda`  function can specify an anonymous function to be performed by `mapcar`. This is useful when some of the function arguments are constant or are supplied by some other means. The following example demonstrates the use of `lambda`  with `mapcar`:

```lisp
(mapcar '(lambda (x)
          (+ x 3)
          )
         '(10 20 30)
)

(13 23 33)
```
