---
title: foreach (AutoLISP)
guid: "GUID-BE6A23C4-4E18-45A6-854E-2DE9574A6925"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BE6A23C4-4E18-45A6-854E-2DE9574A6925.htm"
generated: "2025-11-28T19:06:29.809714Z"
description: Evaluates expressions for all members of a list
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

# foreach (AutoLISP)

> Evaluates expressions for all members of a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BE6A23C4-4E18-45A6-854E-2DE9574A6925.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BE6A23C4-4E18-45A6-854E-2DE9574A6925.htm)
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
(foreach
name list [expr...]
)
```

- ***name*:** **Type:**  Symbol  Variable that each element in the list will be assigned to.
- ***list*:** **Type:**  List  List to be stepped through and evaluated.
- ***expr*:** **Type:**  List  Expression to be evaluated for each element in *list*.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, or nil

The result of the last *expr*  evaluated. If no *expr*  is specified, `foreach`  returns `nil`.

## Remarks

The `foreach`  function steps through a list, assigning each element in the list to a variable, and evaluates each expression for every element in the list. Any number of expressions can be specified.

## Examples

Print each element in a list:

```lisp
(foreach n '(a b c) (print n))

A
B
C C
```

`foreach`  prints each element in the list and returns `C`, the last element. This command is equivalent to the following sequence of commands, except that `foreach`  returns the result of only the last expression evaluated:

```lisp
(print a)
(print b)
(print c)
```
