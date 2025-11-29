---
title: "defun-q-list-ref (AutoLISP)"
guid: "GUID-DAF4A76E-9346-4A68-A0C8-17695177033B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DAF4A76E-9346-4A68-A0C8-17695177033B.htm"
generated: "2025-11-28T19:06:26.315269Z"
description: "Displays the list structure of a function defined with defun-q"
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

# defun-q-list-ref (AutoLISP)

> Displays the list structure of a function defined with defun-q

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DAF4A76E-9346-4A68-A0C8-17695177033B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DAF4A76E-9346-4A68-A0C8-17695177033B.htm)
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
(defun-q-list-ref
'function
)
```

- ***function*:** **Type:**  Symbol  A symbol naming the function.

## Return Values

**Type:**  List, Symbol, or nil

The list definition of the function; otherwise `nil`, if the argument is not a list.

## Examples

Define a function using `defun-q`:

```lisp
(defun-q my-startup (x) (print (list x)))

MY-STARTUP
```

Use `defun-q-list-ref`  to display the list structure of `my-startup`:

```lisp
(defun-q-list-ref 'my-startup)

((X) (PRINT (LIST X)))
```
