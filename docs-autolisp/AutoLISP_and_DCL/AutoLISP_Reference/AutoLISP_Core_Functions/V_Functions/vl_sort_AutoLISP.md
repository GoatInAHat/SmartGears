---
title: "vl-sort (AutoLISP)"
guid: "GUID-F3B27BD2-27FA-4185-B22C-85509175C171"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F3B27BD2-27FA-4185-B22C-85509175C171.htm"
generated: "2025-11-28T19:06:49.873807Z"
description: Sorts the elements in a list according to a given compare function
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

# vl-sort (AutoLISP)

> Sorts the elements in a list according to a given compare function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F3B27BD2-27FA-4185-B22C-85509175C171.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F3B27BD2-27FA-4185-B22C-85509175C171.htm)
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
(vl-sort
lst comparison-function
)
```

- ***lst*:** **Type:**  List  Any list to sort.
- ***comparison-function*:** **Type:**  Subroutine or Symbol  A comparison function. This can be any function that accepts two arguments and returns `T`  (or any non- `nil`  value) if the first argument precedes the second in the sort order. The *comparison-function*  value can take one of the following forms:  A symbol (function name)  `'(LAMBDA (A1 A2) ...)`  `(FUNCTION (LAMBDA (A1 A2) ...))`

## Return Values

**Type:**  List

A list containing the elements of *lst*  in the order specified by *comparison-function*. Duplicate elements may be eliminated from the list.

## Examples

Sort a list of numbers:

```lisp
(vl-sort '(3 2 1 3) '<)

(1 2 3)
```

Note that the result list contains only one 3.

Sort a list of 2D points by *Y*  coordinate:

```lisp
(vl-sort '((1 3) (2 2) (3 1))
             (function (lambda (e1 e2)
                         (< (cadr e1) (cadr e2)))))

((3 1) (2 2) (1 3))
```

Sort a list of symbols:

```lisp
(vl-sort
   '(a d c b a)
   '(lambda (s1 s2)
    (< (vl-symbol-name s1) (vl-symbol-name s2))))

(A B C D)       ;  Note that only one A remains in the result list
```
