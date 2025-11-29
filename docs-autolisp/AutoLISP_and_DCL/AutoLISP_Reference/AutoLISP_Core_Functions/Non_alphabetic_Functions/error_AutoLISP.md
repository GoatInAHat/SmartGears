---
title: *error* (AutoLISP)
guid: "GUID-CF913180-17CC-43C7-B89F-3BC82FFBEFB9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CF913180-17CC-43C7-B89F-3BC82FFBEFB9.htm"
generated: "2025-11-28T19:06:20.160579Z"
description: "A user-definable error-handling function"
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

# *error* (AutoLISP)

> A user-definable error-handling function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CF913180-17CC-43C7-B89F-3BC82FFBEFB9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CF913180-17CC-43C7-B89F-3BC82FFBEFB9.htm)
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
(*error*
string
)
```

- ***string*:** **Type:**  String  A textual string that contains a description of the error and is passed by AutoCAD.

## Return Values

**Type:**  N/A

This function does not return, except when using `vl-exit-with-value`.

## Remarks

If `*error*`  is not `nil`, it is executed as a function whenever an AutoLISP error condition exists.

Your `*error*`  function can include calls to the `command`  function without arguments (for example, `(command)`). This will cancel a previous AutoCAD command called with the `command`  function.

## Examples

The following function does the same thing that the AutoLISP standard error handler does. It prints the text “error: ” followed by a description:

```lisp
(defun *error* (msg)
  (princ "error: ")
  (princ msg)
 (princ)
)
```
