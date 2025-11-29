---
title: "layerstate-has (AutoLISP)"
guid: "GUID-0936B91C-7DE0-49DD-9BCD-FD097417E348"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0936B91C-7DE0-49DD-9BCD-FD097417E348.htm"
generated: "2025-11-28T19:06:34.838383Z"
description: Checks if a layer state is present
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

# layerstate-has (AutoLISP)

> Checks if a layer state is present

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0936B91C-7DE0-49DD-9BCD-FD097417E348.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0936B91C-7DE0-49DD-9BCD-FD097417E348.htm)
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
(layerstate-has
layerstatename
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer state to be queried.

## Return Values

**Type:**  T or nil

`T`  if the name exists; otherwise `nil`.

## Examples

```lisp
(layerstate-has "myLayerState")

T
```
