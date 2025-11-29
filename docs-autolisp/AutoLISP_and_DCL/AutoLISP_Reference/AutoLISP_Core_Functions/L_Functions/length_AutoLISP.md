---
title: length (AutoLISP)
guid: "GUID-DD227383-1895-46B6-A83B-43000FCC1018"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DD227383-1895-46B6-A83B-43000FCC1018.htm"
generated: "2025-11-28T19:06:35.640780Z"
description: Returns an integer indicating the number of elements in a list
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

# length (AutoLISP)

> Returns an integer indicating the number of elements in a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DD227383-1895-46B6-A83B-43000FCC1018.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DD227383-1895-46B6-A83B-43000FCC1018.htm)
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
(length
lst
)
```

- ***lst*:** **Type:**  List  An empty list or a list with one or more elements.

## Return Values

**Type:**  Integer

A numeric value.

## Examples

```lisp
(length '(a b c d))

4

(length '(a b (c d)))

3

(length '())

0
```
