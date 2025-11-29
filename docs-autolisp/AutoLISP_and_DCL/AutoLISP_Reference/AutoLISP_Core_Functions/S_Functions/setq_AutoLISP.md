---
title: setq (AutoLISP)
guid: "GUID-2F4B7A7B-7B6F-4E1C-B32E-677506094EAA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2F4B7A7B-7B6F-4E1C-B32E-677506094EAA.htm"
generated: "2025-11-28T19:06:40.824528Z"
description: Sets the value of a symbol or symbols to associated expressions
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

# setq (AutoLISP)

> Sets the value of a symbol or symbols to associated expressions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2F4B7A7B-7B6F-4E1C-B32E-677506094EAA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2F4B7A7B-7B6F-4E1C-B32E-677506094EAA.htm)
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
(setq
sym expr [sym expr] ...
)
```

- ***sym*:** **Type:**  Symbol  The user-defined variable to assign *expr*  to. This argument is not evaluated.
- ***expr*:** **Type:**  Integer, Real, String, List, File, Ename (entity name), T, or nil  An expression.

## Return Values

**Type:**  Integer, Real, String, List, File, Ename (entity name), T, or nil

The result of the last *expr*  evaluated.

## Remarks

This is the basic assignment function in AutoLISP. The `setq`  function can assign multiple symbols in one call to the function.

## Examples

The following function call sets variable `a`  to 5.0:

```lisp
(setq a 5.0)

5.0
```

Whenever `a`  is evaluated, it returns the real number 5.0.

The following command sets two variables, `b`  and `c`:

```lisp
(setq b 123 c 4.7)

4.7
```

`setq`  returns the value of the last variable set.

In the following example, `s`  is set to a string:

```lisp
(setq s "it")

"it"
```

The following example assigns a list to `x`:

```lisp
(setq x '(a b))

(A B)
```
