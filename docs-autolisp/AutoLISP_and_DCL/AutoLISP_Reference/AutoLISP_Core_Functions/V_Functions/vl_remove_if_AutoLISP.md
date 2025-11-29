---
title: "vl-remove-if (AutoLISP)"
guid: "GUID-9934F630-4697-47BA-84E6-A0430A3346EE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9934F630-4697-47BA-84E6-A0430A3346EE.htm"
generated: "2025-11-28T19:06:49.502159Z"
description: Returns all elements of the supplied list that fail the test function
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

# vl-remove-if (AutoLISP)

> Returns all elements of the supplied list that fail the test function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9934F630-4697-47BA-84E6-A0430A3346EE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9934F630-4697-47BA-84E6-A0430A3346EE.htm)
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
(vl-remove-if
predicate-function lst
)
```

- ***predicate-function*:** **Type:**  Subroutine or Symbol  The test function. This can be any function that accepts a single argument and returns `T`  for any user-specified condition. The *predicate-function*  value can take one of the following forms:  A symbol (function name)  `'(LAMBDA (A1 A2) ...)`  `(FUNCTION (LAMBDA (A1 A2) ...))`
- ***lst*:** **Type:**  List  A list to be tested.

## Return Values

**Type:**  List or nil

A list containing all elements of *lst*  for which *predicate-function*  returns `nil`.

## Examples

```lisp
(vl-remove-if 'vl-symbolp (list pi t 0 "abc"))

(3.14159 0 "abc")
```
