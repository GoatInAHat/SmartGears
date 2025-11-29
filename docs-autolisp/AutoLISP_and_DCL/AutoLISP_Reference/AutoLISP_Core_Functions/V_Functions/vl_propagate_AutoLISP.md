---
title: "vl-propagate (AutoLISP)"
guid: "GUID-9B30560A-5037-4C2D-8175-429E110277F5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9B30560A-5037-4C2D-8175-429E110277F5.htm"
generated: "2025-11-28T19:06:48.995181Z"
description: Copies the value of a variable into all open document namespaces (and sets its value in any subsequent drawings opened during the current AutoCAD session)
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

# vl-propagate (AutoLISP)

> Copies the value of a variable into all open document namespaces (and sets its value in any subsequent drawings opened during the current AutoCAD session)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9B30560A-5037-4C2D-8175-429E110277F5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9B30560A-5037-4C2D-8175-429E110277F5.htm)
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
(vl-propagate
'symbol
)
```

- ***symbol*:** **Type:**  Symbol  A symbol naming an AutoLISP variable.

## Return Values

**Type:**  nil

Always returns `nil`.

## Examples

```lisp
(vl-propagate 'radius)

nil
```
