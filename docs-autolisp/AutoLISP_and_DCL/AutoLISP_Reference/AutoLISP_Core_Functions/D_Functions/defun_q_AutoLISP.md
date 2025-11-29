---
title: "defun-q (AutoLISP)"
guid: "GUID-5EE138EF-D531-441B-9F12-8D5645C540FE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5EE138EF-D531-441B-9F12-8D5645C540FE.htm"
generated: "2025-11-28T19:06:26.632870Z"
description: Defines a function as a list
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

# defun-q (AutoLISP)

> Defines a function as a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5EE138EF-D531-441B-9F12-8D5645C540FE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5EE138EF-D531-441B-9F12-8D5645C540FE.htm)
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
(defun-q
sym ([arguments] [/ variables ...]) expr ...
)
```

- ***sym*:** **Type:**  Symbol  A symbol naming the function.
- ***arguments*:** **Type:**  Integer, Real, String, List, T, or nil  The names of arguments expected by the function.
- ***/ variables*:** **Type:**  Symbol  The names of one or more local variables for the function.  The slash preceding the variable names must be separated from the first local name and from the last argument, if any, by at least one space.
- ***expr*:** **Type:**  List  Any number of AutoLISP expressions to be evaluated when the function executes.

## Return Values

**Type:**  List or Symbol

The result of the last expression evaluated.

## Remarks

The `defun-q`  function is provided strictly for backward-compatibility with previous versions of AutoLISP, and should not be used for other purposes. You can use `defun-q`  in situations where you need to access a function definition as a list structure, which is the way `defun`  was implemented in previous, non-compiled versions of AutoLISP.

If you do not declare any arguments or local symbols, you must supply an empty set of parentheses after the function name.

If duplicate argument or symbol names are specified, AutoLISP uses the first occurrence of each name and ignores the following occurrences.

## Examples

```lisp
(defun-q my-startup (x) (print (list x)))

MY-STARTUP

(my-startup 5)

(5) (5)
```

Use `defun-q-list-ref`  to display the list structure of `my-startup`:

```lisp
(defun-q-list-ref 'my-startup)

((X) (PRINT (LIST X)))
```
