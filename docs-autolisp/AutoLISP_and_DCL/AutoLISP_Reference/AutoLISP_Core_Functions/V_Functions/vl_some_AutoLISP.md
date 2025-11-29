---
title: "vl-some (AutoLISP)"
guid: "GUID-2840F793-DA88-4140-8A9D-EBAC47B62F9D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2840F793-DA88-4140-8A9D-EBAC47B62F9D.htm"
generated: "2025-11-28T19:06:49.761475Z"
description: Checks whether the predicate is not nil for one element combination
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

# vl-some (AutoLISP)

> Checks whether the predicate is not nil for one element combination

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2840F793-DA88-4140-8A9D-EBAC47B62F9D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2840F793-DA88-4140-8A9D-EBAC47B62F9D.htm)
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
(vl-some
predicate-function lst [lst ...]
)
```

- ***predicate-function*:** **Type:**  Subroutine or Symbol  The test function. This can be any function that accepts as many arguments as there are lists provided with `vl-some`, and returns `T`  on a user-specified condition. The *predicate-function*  value can take one of the following forms:  A symbol (function name)  `'(LAMBDA (A1 A2) ...)`  `(FUNCTION (LAMBDA (A1 A2) ...))`
- ***lst*:** **Type:**  List  A list to be tested.

## Return Values

**Type:**  List or nil

The predicate value, if *predicate-function*  returned a value other than `nil`; otherwise `nil`.

## Remarks

The `vl-some`  function passes the first element of each supplied list as an argument to the test function, then the next element from each list, and so on. Evaluation stops as soon as the predicate function returns a non- `nil`  value for an argument combination, or until all elements have been processed in one of the lists.

## Examples

The following example checks whether `nlst`  (a number list) has equal elements in sequence:

```lisp
(setq nlst (list 0 2 pi pi 4))

(0 2 3.14159 3.14159 4)

(vl-some '= nlst (cdr nlst))

T
```
