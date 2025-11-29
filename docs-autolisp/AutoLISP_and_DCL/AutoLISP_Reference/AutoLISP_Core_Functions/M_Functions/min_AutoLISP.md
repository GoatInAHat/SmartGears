---
title: min (AutoLISP)
guid: "GUID-F401AD55-57B2-4B1A-9704-D7D8D45AE465"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F401AD55-57B2-4B1A-9704-D7D8D45AE465.htm"
generated: "2025-11-28T19:06:37.237526Z"
description: Returns the smallest of the numbers given
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

# min (AutoLISP)

> Returns the smallest of the numbers given

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F401AD55-57B2-4B1A-9704-D7D8D45AE465.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F401AD55-57B2-4B1A-9704-D7D8D45AE465.htm)
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
(min
[number number ...]
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

A numeric value. If any *number*  argument is a real, a real is returned; otherwise, an integer is returned. If no argument is supplied, `min`  returns 0.

## Examples

```lisp
(min 683 -10.0)

-10.0

(min 73 2 48 5)

2

(min 73.0 2 48 5)

2.0

(min 2 4 6.7)

2.0
```
