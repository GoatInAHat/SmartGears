---
title: itoa (AutoLISP)
guid: "GUID-7E247CA3-95D3-4497-BDE2-6EB0E727DD4D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E247CA3-95D3-4497-BDE2-6EB0E727DD4D.htm"
generated: "2025-11-28T19:06:33.662095Z"
description: Returns the conversion of an integer into a string
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

# itoa (AutoLISP)

> Returns the conversion of an integer into a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E247CA3-95D3-4497-BDE2-6EB0E727DD4D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E247CA3-95D3-4497-BDE2-6EB0E727DD4D.htm)
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
(itoa
int
)
```

- ***int*:** **Type:**  Integer  A numeric value.

## Return Values

**Type:**  String

A string derived from *int*.

## Examples

```lisp
(itoa 33)

"33"

(itoa -17)

"-17"
```
