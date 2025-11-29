---
title: layoutlist (AutoLISP)
guid: "GUID-AE3F338F-0B11-45B0-AB12-2C119203A977"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AE3F338F-0B11-45B0-AB12-2C119203A977.htm"
generated: "2025-11-28T19:06:35.564674Z"
description: Returns a list of all paper space layouts in the current drawing
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

# layoutlist (AutoLISP)

> Returns a list of all paper space layouts in the current drawing

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AE3F338F-0B11-45B0-AB12-2C119203A977.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AE3F338F-0B11-45B0-AB12-2C119203A977.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows and Mac OS only

## Signature

```lisp
(layoutlist)
```

No arguments.

## Return Values

**Type:**  List

A list of strings.

## Examples

```lisp
(layoutlist)

("Layout1" "Layout2")
```
