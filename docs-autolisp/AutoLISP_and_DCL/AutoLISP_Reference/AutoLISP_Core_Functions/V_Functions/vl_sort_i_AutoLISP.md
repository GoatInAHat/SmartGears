---
title: "vl-sort-i (AutoLISP)"
guid: "GUID-B21C91DD-D359-4370-84ED-317F8BBAF414"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B21C91DD-D359-4370-84ED-317F8BBAF414.htm"
generated: "2025-11-28T19:06:50.014225Z"
description: Sorts the elements in a list according to a given compare function, and returns the element index numbers
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

# vl-sort-i (AutoLISP)

> Sorts the elements in a list according to a given compare function, and returns the element index numbers

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B21C91DD-D359-4370-84ED-317F8BBAF414.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B21C91DD-D359-4370-84ED-317F8BBAF414.htm)
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
(vl-sort-i
lst comparison-function
)
```

- ***lst*:** **Type:**  List  Any list to sort.
- ***comparison-function*:** **Type:**  Subroutine or Symbol  A comparison function. This can be any function that accepts two arguments and returns `T`  (or any non- `nil`  value) if the first argument precedes the second in the sort order. The *comparison-function*  value can take one of the following forms:  A symbol (function name)  `'(LAMBDA (A1 A2) ...)`  `(FUNCTION (LAMBDA (A1 A2) ...))`

## Return Values

**Type:**  List

A list containing the index values of the elements of *lst*, sorted in the order specified by *comparison-function*. Duplicate elements will be retained in the result.

## Examples

Sort a list of characters in descending order:

```lisp
(vl-sort-i '("a" "d" "f" "c") '>)

(2 1 3 0)
```

The sorted list order is “f” “d” “c” “a”; “f” is the 3rd element (index 2) in the original list, “d” is the 2nd element (index 1) in the list, and so on.

Sort a list of numbers in ascending order:

```lisp
(vl-sort-i '(3 2 1 3) '<)

(2 1 3 0)
```

Note that both occurrences of 3 are accounted for in the result list.

Sort a list of 2D points by *Y*  coordinate:

```lisp
(vl-sort-i '((1 3) (2 2) (3 1))
         (function (lambda (e1 e2)
                (< (cadr e1) (cadr e2)))))

(2 1 0)
```

Sort a list of symbols:

```lisp
(vl-sort-i
   '(a d c b a)
   '(lambda (s1 s2)
    (< (vl-symbol-name s1) (vl-symbol-name s2))))

(4 0 3 2 1)
```

Note that both `a`'s are accounted for in the result list.
