---
title: "vl-exit-with-value (AutoLISP)"
guid: "GUID-80622A39-A5E8-4E68-824E-E66BD8D3E9DE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-80622A39-A5E8-4E68-824E-E66BD8D3E9DE.htm"
generated: "2025-11-28T19:06:46.628504Z"
description: Returns a value to the function that invoked the *error* handler from another namespace
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

# vl-exit-with-value (AutoLISP)

> Returns a value to the function that invoked the *error* handler from another namespace

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-80622A39-A5E8-4E68-824E-E66BD8D3E9DE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-80622A39-A5E8-4E68-824E-E66BD8D3E9DE.htm)
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
(vl-exit-with-value
value
)
```

- ***value*:** **Type:**  Integer, Real, String, List, File, Ename (entity name), T, or nil  Any value.

## Return Values

**Type:**  Integer, Real, String, List, File, Ename (entity name), T, or nil

*value*  provided to the function.

## Remarks

An `*error*`  handler can use the `vl-exit-with-value`  function to return a value to the program that called the function.

## Examples

The following example uses `vl-exit-with-value`  to return the integer value 3 to the function that invoked the VLX:

```lisp
(defun *error* (msg)
  ... ; processing in VLX-T namespace/execution context
  (vl-exit-with-value  3))
```
