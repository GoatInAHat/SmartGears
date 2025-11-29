---
title: subst (AutoLISP)
guid: "GUID-25214E69-090A-45C3-8210-6D9801255E44"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-25214E69-090A-45C3-8210-6D9801255E44.htm"
generated: "2025-11-28T19:06:43.058433Z"
description: Searches a list for an old item and returns a copy of the list with a new item substituted in place of every occurrence of the old item
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

# subst (AutoLISP)

> Searches a list for an old item and returns a copy of the list with a new item substituted in place of every occurrence of the old item

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-25214E69-090A-45C3-8210-6D9801255E44.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-25214E69-090A-45C3-8210-6D9801255E44.htm)
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
(subst
newitem olditem lst
)
```

- ***newitem*:** **Type:**  Integer, Real, String, List, Symbol, Ename (entity name), T, or nil  An atom or list.
- ***olditem*:** **Type:**  Integer, Real, String, List, Symbol, Ename (entity name), T, or nil  An atom or list.
- ***lst*:** **Type:**  List  A list.

## Return Values

A list, with *newitem*  replacing all occurrences of *olditem*. If *olditem*  is not found in *lst*, `subst`  returns *lst*  unchanged.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## Examples

```lisp
(setq sample '(a b (c d) b))

(A B (C D) B)

(subst 'qq 'b sample)

(A QQ (C D) QQ)

(subst 'qq 'z sample)

(A B (C D) B)

(subst 'qq '(c d) sample)

(A B QQ B)

(subst '(qq rr) '(c d) sample)

(A B (QQ RR) B)

(subst '(qq rr) 'z sample)

(A B (C D) B)
```

When used in conjunction with `assoc`, `subst`  provides a convenient means of replacing the value associated with one key in an association list, as demonstrated by the following function calls.

Set variable `who`  to an association list:

```lisp
(setq who '((first john) (mid q) (last public)))

((FIRST JOHN) (MID Q) (LAST PUBLIC))
```

The following sets `old`  to (FIRST JOHN) and `new`  to (FIRST J):

```lisp
(setq old (assoc 'first who) new '(first j))

(FIRST J)
```

Finally, replace the value of the first item in the association list:

```lisp
(subst new old who)

((FIRST J) (MID Q) (LAST PUBLIC))
```
