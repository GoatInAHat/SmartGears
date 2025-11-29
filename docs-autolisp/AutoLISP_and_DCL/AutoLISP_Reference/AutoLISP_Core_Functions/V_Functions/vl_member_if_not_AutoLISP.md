---
title: "vl-member-if-not (AutoLISP)"
guid: "GUID-27EA5788-B68D-4302-900F-9D99902025E4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-27EA5788-B68D-4302-900F-9D99902025E4.htm"
generated: "2025-11-28T19:06:48.463935Z"
description: Determines if the predicate is nil for one of the list members
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

# vl-member-if-not (AutoLISP)

> Determines if the predicate is nil for one of the list members

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-27EA5788-B68D-4302-900F-9D99902025E4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-27EA5788-B68D-4302-900F-9D99902025E4.htm)
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
(vl-member-if-not
predicate-function lst
)
```

- ***predicate-function*:** **Type:**  Subroutine or Symbol  The test function. This can be any function that accepts a single argument and returns `T`  for any user-specified condition. The *predicate-function*  value can take one of the following forms:  A symbol (function name)  `'(LAMBDA (A1 A2) ...)`  `(FUNCTION (LAMBDA (A1 A2) ...))`
- ***lst*:** **Type:**  List  A list to be tested.

## Return Values

**Type:**  List or nil

A list, starting with the first element that fails the test and containing all elements following this in the original argument. If none of the elements fails the test condition, `vl-member-if-not`  returns `nil`.

## Remarks

The `vl-member-if-not`  function passes each element in *lst*  to the function specified in *predicate-function*. If the function returns `nil`, `vl-member-if-not`  returns the rest of the list in the same manner as the `member`  function.

## Examples

```lisp
(vl-member-if-not 'atom '(1 "Str" (0 . "line") nil t))

((0 . "line") nil T)
```
