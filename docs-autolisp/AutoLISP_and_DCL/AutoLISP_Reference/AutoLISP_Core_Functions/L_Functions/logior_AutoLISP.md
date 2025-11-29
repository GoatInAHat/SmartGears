---
title: logior (AutoLISP)
guid: "GUID-A4BF3B68-4988-42F0-AFE6-BFD06D0DB159"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A4BF3B68-4988-42F0-AFE6-BFD06D0DB159.htm"
generated: "2025-11-28T19:06:36.380475Z"
description: Returns the result of the logical bitwise inclusive OR of a list of integers
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

# logior (AutoLISP)

> Returns the result of the logical bitwise inclusive OR of a list of integers

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A4BF3B68-4988-42F0-AFE6-BFD06D0DB159.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A4BF3B68-4988-42F0-AFE6-BFD06D0DB159.htm)
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
(logior
[int int ...]
)
```

- ***int*:** **Type:**  Integer  A numeric value.

## Return Values

**Type:**  Integer

A numeric value (0, if no arguments are supplied).

## Examples

```lisp
(logior 1 2 4)

7

(logior 9 3)

11
```
