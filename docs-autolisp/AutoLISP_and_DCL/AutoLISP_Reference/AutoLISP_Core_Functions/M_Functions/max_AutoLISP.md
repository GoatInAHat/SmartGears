---
title: max (AutoLISP)
guid: "GUID-057C56D3-8A15-406B-95CA-68DD859E64FB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-057C56D3-8A15-406B-95CA-68DD859E64FB.htm"
generated: "2025-11-28T19:06:36.652318Z"
description: Returns the largest of the numbers given
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

# max (AutoLISP)

> Returns the largest of the numbers given

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-057C56D3-8A15-406B-95CA-68DD859E64FB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-057C56D3-8A15-406B-95CA-68DD859E64FB.htm)
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
(max
[number number ...]
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

A number. If any of the arguments are real numbers, a real is returned; otherwise an integer is returned. If no argument is supplied, `max`  returns 0.

## Examples

```lisp
(max 4.07 -144)

4.07

(max -88 19 5 2)

19

(max 2.1 4 8)

8.0
```
