---
title: atoi (AutoLISP)
guid: "GUID-20EF237C-7079-4E29-860C-B8531D6C7F36"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-20EF237C-7079-4E29-860C-B8531D6C7F36.htm"
generated: "2025-11-28T19:06:24.297572Z"
description: Converts a string into an integer
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

# atoi (AutoLISP)

> Converts a string into an integer

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-20EF237C-7079-4E29-860C-B8531D6C7F36.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-20EF237C-7079-4E29-860C-B8531D6C7F36.htm)
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
(atoi
str
)
```

- ***str*:** **Type:**  String  A string to be converted into an integer.

## Return Values

**Type:**  Integer

A numeric value.

## Examples

```lisp
(atoi "97")

97

(atoi "3")

3

(atoi "3.9")

3
```
