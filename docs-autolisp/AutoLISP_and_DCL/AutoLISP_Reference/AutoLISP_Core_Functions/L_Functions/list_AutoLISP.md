---
title: list (AutoLISP)
guid: "GUID-2BA66308-DE64-4BA6-8F57-6E3EC1A93141"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2BA66308-DE64-4BA6-8F57-6E3EC1A93141.htm"
generated: "2025-11-28T19:06:35.728249Z"
description: Takes any number of expressions and combines them into one list
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

# list (AutoLISP)

> Takes any number of expressions and combines them into one list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2BA66308-DE64-4BA6-8F57-6E3EC1A93141.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2BA66308-DE64-4BA6-8F57-6E3EC1A93141.htm)
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
(list
[expr ...]
)
```

- ***expr*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  An AutoLISP expression.

## Return Values

**Type:**  List or nil

A list, unless no expressions are supplied, in which case `list`  returns `nil`.

## Remarks

This function is frequently used to define a 2D or 3D point variable (a list of two or three reals).

## Examples

```lisp
(list 'a 'b 'c)

(A B C)

(list 'a '(b c) 'd)

(A (B C) D)

(list 3.9 6.7)

(3.9 6.7)
```

As an alternative to using the `list`  function, you can explicitly quote a list with the `quote`  function if there are no variables or undefined items in the list. The single quote character (`'`) is defined as the quote function.

```lisp
'(3.9 6.7)

means the same as
 (list 3.9 6.7)
```

This can be useful for creating association lists and defining points.
