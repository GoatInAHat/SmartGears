---
title: cond (AutoLISP)
guid: "GUID-7BA45202-D95F-4F2D-8D83-965024826074"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7BA45202-D95F-4F2D-8D83-965024826074.htm"
generated: "2025-11-28T19:06:25.959740Z"
description: Serves as the primary conditional function for AutoLISP
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

# cond (AutoLISP)

> Serves as the primary conditional function for AutoLISP

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7BA45202-D95F-4F2D-8D83-965024826074.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7BA45202-D95F-4F2D-8D83-965024826074.htm)
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
(cond
[((test) (result ...)) ...]
)
```

- ***test*:** **Type:**  List  Test condition to be evaluated.
- ***result*:** **Type:**  List  Arguments that are executed if the test condition is successful.

## Return Values

**Type:**  T or nil

The value of the last expression in the sublist. If there is only one expression in the sublist (that is, if *result*  is missing), the value of the *test*  expression is returned. If no arguments are supplied, `cond`  returns `nil`.

## Remarks

The `cond`  function accepts any number of lists as arguments. It evaluates the first item in each list (in the order supplied) until one of these items returns a value other than `nil`. It then evaluates those expressions that follow the test that succeeded.

## Examples

The following example uses `cond`  to perform an absolute value calculation:

```lisp
(cond
   ((minusp a) (- a))
   (t a)
)
```

If the variable `a`  is set to the value-10, this returns 10.

As shown, `cond`  can be used as a *case*  type function. It is common to use `T`  as the last (default) *test*  expression. Here's another simple example. Given a user response string in the variable `s`, this function tests the response and returns 1 if it is `Y`  or `y`, 0 if it is `N`  or `n`; otherwise `nil`.

```lisp
(cond
   ((= s "Y") 1)
   ((= s "y") 1)
   ((= s "N") 0)
   ((= s "n") 0)
   (t nil)
)
```
