---
title: "layerstate-getlastrestored (AutoLISP)"
guid: "GUID-404D79AB-B213-4EF4-A0A6-19F2E4F0A9A9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-404D79AB-B213-4EF4-A0A6-19F2E4F0A9A9.htm"
generated: "2025-11-28T19:06:34.440144Z"
description: Returns the name of the last restored layer state in the current drawing
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

# layerstate-getlastrestored (AutoLISP)

> Returns the name of the last restored layer state in the current drawing

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-404D79AB-B213-4EF4-A0A6-19F2E4F0A9A9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-404D79AB-B213-4EF4-A0A6-19F2E4F0A9A9.htm)
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
(layerstate-getlastrestored)
```

No arguments.

## Return Values

**Type:**  String or nil

Returns the name of the last restored layer state in the current drawing.

## Examples

```lisp
(layerstate-getlastrestored)

"Foundation"
```
