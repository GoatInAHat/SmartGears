---
title: prompt (AutoLISP)
guid: "GUID-2F702ECF-FEC7-4DEE-8575-88CDEE05F55D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2F702ECF-FEC7-4DEE-8575-88CDEE05F55D.htm"
generated: "2025-11-28T19:06:39.067803Z"
description: Displays a string on your screen's prompt area
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

# prompt (AutoLISP)

> Displays a string on your screen's prompt area

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2F702ECF-FEC7-4DEE-8575-88CDEE05F55D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2F702ECF-FEC7-4DEE-8575-88CDEE05F55D.htm)
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
(prompt
msg
)
```

- ***msg*:** **Type:**  String  Message to be displayed to the user.

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

On dual-screen AutoCAD configurations, `prompt`  displays *msg*  on both screens and is, therefore, preferable to `princ`.

## Examples

```lisp
(prompt "New value: ")

New value: nil
```
