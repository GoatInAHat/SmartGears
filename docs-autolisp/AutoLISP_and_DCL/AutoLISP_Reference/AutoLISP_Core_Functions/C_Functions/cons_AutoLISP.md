---
title: cons (AutoLISP)
guid: "GUID-33B418E7-DB3D-4CBE-954E-F070F0A7CB2B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33B418E7-DB3D-4CBE-954E-F070F0A7CB2B.htm"
generated: "2025-11-28T19:06:26.031049Z"
description: Adds an element to the beginning of a list, or constructs a dotted list
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

# cons (AutoLISP)

> Adds an element to the beginning of a list, or constructs a dotted list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33B418E7-DB3D-4CBE-954E-F070F0A7CB2B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33B418E7-DB3D-4CBE-954E-F070F0A7CB2B.htm)
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
(cons
new-first-element list-or-atom
)
```

- ***new-first-element*:** **Type:**  Integer, Real, String, List, T, or nil  Element to be added to the beginning of a list. This element can be an atom or a list.
- ***list-or-atom*:** **Type:**  Integer, Real, String, List, or T  A list or an atom.

## Return Values

**Type:**  List

The value returned depends on the data type of *list-or-atom*. If *list-or-atom*  is a list, `cons`  returns that list with *new-first-element*  added as the first item in the list. If *list-or-atom*  is an atom, `cons`  returns a dotted pair consisting of *new-first-element*  and *list-or-atom*.

## Examples

```lisp
(cons 'a '(b c d))

(A B C D)

(cons '(a) '(b c d))

((A) B C D)

(cons 'a 2)

(A . 2)
```
