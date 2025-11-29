---
title: defun (AutoLISP)
guid: "GUID-5269529D-A013-4AB4-AAB7-DBA1C7CA73EB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5269529D-A013-4AB4-AAB7-DBA1C7CA73EB.htm"
generated: "2025-11-28T19:06:26.720640Z"
description: Defines a function
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

# defun (AutoLISP)

> Defines a function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5269529D-A013-4AB4-AAB7-DBA1C7CA73EB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5269529D-A013-4AB4-AAB7-DBA1C7CA73EB.htm)
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
(defun
sym ([arguments] [/ variables ...]) expr ...
)
```

- ***sym*:** **Type:**  Symbol  A symbol naming the function.
- ***arguments*:** **Type:**  Integer, Real, String, List, T, or nil  The names of arguments expected by the function.
- ***/ variables*:** **Type:**  Symbol  The names of one or more local variables for the function.  The slash preceding the variable names must be separated from the first local name and from the last argument, if any, by at least one space.
- ***expr*:** **Type:**  List  Any number of AutoLISP expressions to be evaluated when the function executes.

## Return Values

The result of the last expression evaluated.

Caution:
 Never use the name of a built-in function or symbol for the
sym
 argument to
defun
. This overwrites the original definition and makes the built-in function or symbol inaccessible. To get a list of built-in and previously defined functions, use the
atoms-family
 function.

## Remarks

If you do not declare any arguments or local symbols, you must supply an empty set of parentheses after the function name.

If duplicate argument or symbol names are specified, AutoLISP uses the first occurrence of each name and ignores the following occurrences.

## Examples

```lisp
(defun myfunc (x y) ...)
Function takes two arguments

(defun myfunc (/ a b) ...)
Function has two local variables

(defun myfunc (x / temp) ...)
One argument, one local variable

(defun myfunc () ...)
No arguments or local variables
```
