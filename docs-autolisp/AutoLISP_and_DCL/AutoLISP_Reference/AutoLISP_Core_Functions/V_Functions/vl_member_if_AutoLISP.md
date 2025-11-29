---
title: "vl-member-if (AutoLISP)"
guid: "GUID-CFEA98F0-F6DE-4FF4-839A-0F6C8FBE1319"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CFEA98F0-F6DE-4FF4-839A-0F6C8FBE1319.htm"
generated: "2025-11-28T19:06:48.347489Z"
description: Determines if the predicate is true for one of the list members
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

# vl-member-if (AutoLISP)

> Determines if the predicate is true for one of the list members

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CFEA98F0-F6DE-4FF4-839A-0F6C8FBE1319.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CFEA98F0-F6DE-4FF4-839A-0F6C8FBE1319.htm)
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
(vl-member-if
predicate-function lst
)
```

- ***predicate-function*:** **Type:**  Subroutine or Symbol  The test function. This can be any function that accepts a single argument and returns `T`  for any user-specified condition. The *predicate-function*  value can take one of the following forms:  A symbol (function name)  `'(LAMBDA (A1 A2) ...)`  `(FUNCTION (LAMBDA (A1 A2) ...))`
- ***lst*:** **Type:**  List  A list to be tested.

## Return Values

**Type:**  List or nil

A list, starting with the first element that passes the test and containing all elements following this in the original argument. If none of the elements passes the test condition, `vl-member-if`  returns `nil`.

## Remarks

The `vl-member-if`  function passes each element in *lst*  to the function specified in *predicate-function*. If *predicate-function*  returns a non- `nil`  value, `vl-member-if`  returns the rest of the list in the same manner as the `member`  function.

## Examples

The following command draws a line:

```lisp
(command "._line" '(0 10) '(30 50) nil)

nil
```

The following command uses `vl-member-if`  to return association lists describing an entity, if the entity is a line:

```lisp
(vl-member-if
  '(lambda (x) (= (cdr x) "AcDbLine"))
   (entget (entlast)))

((100 . "AcDbLine") (10 0.0 10.0 0.0) (11 30.0 50.0 0.0) (210 0.0 0.0 1.0))
```
