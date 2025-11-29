---
title: reverse (AutoLISP)
guid: "GUID-7669E8F1-2A4F-42C7-AAA8-74D1300F9744"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7669E8F1-2A4F-42C7-AAA8-74D1300F9744.htm"
generated: "2025-11-28T19:06:40.053350Z"
description: Returns a copy of a list with its elements reversed
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

# reverse (AutoLISP)

> Returns a copy of a list with its elements reversed

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7669E8F1-2A4F-42C7-AAA8-74D1300F9744.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7669E8F1-2A4F-42C7-AAA8-74D1300F9744.htm)
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
(reverse
lst
)
```

- ***lst*:** **Type:**  List  A list of the items to reverse.

## Return Values

**Type:**  List

Values of the *lst*  argument reversed.

## Examples

```lisp
(reverse '((a) b c))

(C B (A))
```
