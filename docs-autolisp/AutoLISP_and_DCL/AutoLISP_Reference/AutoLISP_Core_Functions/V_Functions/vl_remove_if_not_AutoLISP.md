---
title: "vl-remove-if-not (AutoLISP)"
guid: "GUID-53D12042-8DE3-4DAA-83BD-8ABB376ACA97"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-53D12042-8DE3-4DAA-83BD-8ABB376ACA97.htm"
generated: "2025-11-28T19:06:49.596310Z"
description: Returns all elements of the supplied list that pass the test function
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

# vl-remove-if-not (AutoLISP)

> Returns all elements of the supplied list that pass the test function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-53D12042-8DE3-4DAA-83BD-8ABB376ACA97.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-53D12042-8DE3-4DAA-83BD-8ABB376ACA97.htm)
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
(vl-remove-if-not
predicate-function lst
)
```

- ***predicate-function*:** **Type:**  Subroutine or Symbol  The test function. This can be any function that accepts a single argument and returns `T`  for any user-specified condition. The *predicate-function*  value can take one of the following forms:  A symbol (function name)  `'(LAMBDA (A1 A2) ...)`  `(FUNCTION (LAMBDA (A1 A2) ...))`
- ***lst*:** **Type:**  List  A list to be tested.

## Return Values

**Type:**  List or nil

A list containing all elements of *lst*  for which *predicate-function*  returns a non- `nil`  value

## Examples

```lisp
(vl-remove-if-not 'vl-symbolp (list pi t 0 "abc"))

(T)
```
