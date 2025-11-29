---
title: atof (AutoLISP)
guid: "GUID-8618A388-E4CF-40E1-813B-057367DD1840"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8618A388-E4CF-40E1-813B-057367DD1840.htm"
generated: "2025-11-28T19:06:24.125989Z"
description: Converts a string into a real number
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

# atof (AutoLISP)

> Converts a string into a real number

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8618A388-E4CF-40E1-813B-057367DD1840.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8618A388-E4CF-40E1-813B-057367DD1840.htm)
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
(atof
str
)
```

- ***str*:** **Type:**  String  A string to be converted into a real number.

## Return Values

**Type:**  Real

A numeric value.

## Examples

```lisp
(atof "97.1")

97.1

(atof "3")

3.0

(atof "3.9")

3.9
```
