---
title: ~ (bitwise NOT) (AutoLISP)
guid: "GUID-98811E38-FCA8-4ADB-858C-60C3828D7DB4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-98811E38-FCA8-4ADB-858C-60C3828D7DB4.htm"
generated: "2025-11-28T19:06:20.829943Z"
description: Returns the bitwise NOT (1's complement) of the argument
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

# ~ (bitwise NOT) (AutoLISP)

> Returns the bitwise NOT (1's complement) of the argument

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-98811E38-FCA8-4ADB-858C-60C3828D7DB4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-98811E38-FCA8-4ADB-858C-60C3828D7DB4.htm)
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
(~
int
)
```

- ***int*:** **Type:**  Integer  A positive or negative integer value.

## Return Values

**Type:**  Integer

The bitwise NOT (1's complement) of the argument.

## Examples

```lisp
(~ 3)

-4

(~ 100)

-101

(~ -4)

3
```
