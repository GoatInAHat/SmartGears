---
title: "vl-catch-all-apply (AutoLISP)"
guid: "GUID-E08CC2A6-787A-422F-8BD3-18812996794C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E08CC2A6-787A-422F-8BD3-18812996794C.htm"
generated: "2025-11-28T19:06:45.318914Z"
description: Passes a list of arguments to a specified function and traps any exceptions
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

# vl-catch-all-apply (AutoLISP)

> Passes a list of arguments to a specified function and traps any exceptions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E08CC2A6-787A-422F-8BD3-18812996794C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E08CC2A6-787A-422F-8BD3-18812996794C.htm)
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
(vl-catch-all-apply
'function list
)
```

- ***'function*:** **Type:**  Symbol  A function. The *function*  argument can be either a symbol identifying a `defun`  or `lambda`  expression.
- ***list*:** **Type:**  List  A list containing arguments to be passed to the function.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, nil, or catch-all-apply-error

The result of the function call, if successful. If an error occurs, `vl-catch-all-apply`  returns an error object.

## Examples

If the function invoked by `vl-catch-all-apply`  completes successfully, it is the same as using `apply`, as the following examples show:

```lisp
(setq catchit (apply '/ '(50 5)))

10

(setq catchit (vl-catch-all-apply '/ '(50 5)))

10
```

The benefit of using `vl-catch-all-apply`  is that it allows you to intercept errors and continue processing. See what happens when you try to divide by zero using `apply`:

```lisp
(setq catchit (apply '/ '(50 0)))

; error: divide by zero
```

When you use `apply`, an exception occurs and an error message displays. Here is the same operation using `vl-catch-all-apply`:

```lisp
(setq catchit (vl-catch-all-apply '/ '(50 0)))

#<%catch-all-apply-error%>
```

The `vl-catch-all-apply`  function traps the error and returns an error object. Use `vl-catch-all-error-message`  to see the error message contained in the error object:

```lisp
(vl-catch-all-error-message catchit)

"divide by zero"
```
