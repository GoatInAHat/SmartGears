---
title: last (AutoLISP)
guid: "GUID-463C97F2-B72F-44C6-B19B-659D231AA836"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-463C97F2-B72F-44C6-B19B-659D231AA836.htm"
generated: "2025-11-28T19:06:33.892466Z"
description: Returns the last element in a list
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

# last (AutoLISP)

> Returns the last element in a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-463C97F2-B72F-44C6-B19B-659D231AA836.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-463C97F2-B72F-44C6-B19B-659D231AA836.htm)
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
(last
lst
)
```

- ***lst*:** **Type:**  List  A list of one or more elements.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, or nil

An atom or a list.

## Examples

```lisp
(last '(a b c d e))

E

(last '(a b c (d e)))

(D E)
```
