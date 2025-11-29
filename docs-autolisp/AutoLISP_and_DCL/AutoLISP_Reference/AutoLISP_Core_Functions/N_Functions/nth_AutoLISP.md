---
title: nth (AutoLISP)
guid: "GUID-0330FE17-6E15-4E34-BB50-E9040EABDADB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0330FE17-6E15-4E34-BB50-E9040EABDADB.htm"
generated: "2025-11-28T19:06:37.848662Z"
description: Returns the nth element of a list
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

# nth (AutoLISP)

> Returns the nth element of a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0330FE17-6E15-4E34-BB50-E9040EABDADB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0330FE17-6E15-4E34-BB50-E9040EABDADB.htm)
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
(nth
n lst
)
```

- ***n*:** **Type:**  Integer  The number of the element to return from the list (zero is the first element).
- ***lst*:** **Type:**  List  The list with one or more elements.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, or nil

The *n* th element of *lst*. If *n*  is greater than the highest element number of *lst*, `nth`  returns `nil`.

## Examples

```lisp
(nth 3 '(a b c d e))

D

(nth 0 '(a b c d e))

A

(nth 5 '(a b c d e))

nil
```
