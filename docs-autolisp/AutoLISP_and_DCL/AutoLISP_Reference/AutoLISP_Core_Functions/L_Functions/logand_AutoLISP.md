---
title: logand (AutoLISP)
guid: "GUID-4E042C63-8BD7-4FED-B9E1-B5CE12D18273"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E042C63-8BD7-4FED-B9E1-B5CE12D18273.htm"
generated: "2025-11-28T19:06:36.303438Z"
description: Returns the result of the logical bitwise AND of a list of integers
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

# logand (AutoLISP)

> Returns the result of the logical bitwise AND of a list of integers

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E042C63-8BD7-4FED-B9E1-B5CE12D18273.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E042C63-8BD7-4FED-B9E1-B5CE12D18273.htm)
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
(logand
[int int ...]
)
```

- ***int*:** **Type:**  Integer  A numeric value.

## Return Values

**Type:**  Integer

An numeric value (0, if no arguments are supplied).

## Examples

```lisp
(logand 7 15 3)

3

(logand 2 3 15)

2

(logand 8 3 4)

0
```
