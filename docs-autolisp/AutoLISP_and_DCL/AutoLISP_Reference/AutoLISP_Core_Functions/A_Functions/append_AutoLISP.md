---
title: append (AutoLISP)
guid: "GUID-952B175A-D565-43A4-9208-0E6A27A5E742"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-952B175A-D565-43A4-9208-0E6A27A5E742.htm"
generated: "2025-11-28T19:06:23.040507Z"
description: Takes any number of lists and appends them together as one list
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

# append (AutoLISP)

> Takes any number of lists and appends them together as one list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-952B175A-D565-43A4-9208-0E6A27A5E742.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-952B175A-D565-43A4-9208-0E6A27A5E742.htm)
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
(append
[list ...]
)
```

- ***list*:** **Type:**  List  A list.

## Return Values

**Type:**  List or nil

A list with all arguments appended to the original. If no arguments are supplied, `append`  returns `nil`.

## Examples

```lisp
(append '(a b) '(c d))

(A B C D)

(append '((a)(b)) '((c)(d)))

((A) (B) (C) (D))
```
