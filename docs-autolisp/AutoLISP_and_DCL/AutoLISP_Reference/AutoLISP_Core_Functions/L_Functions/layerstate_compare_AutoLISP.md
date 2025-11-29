---
title: "layerstate-compare (AutoLISP)"
guid: "GUID-C2555813-0DD3-46AA-B22F-3F0D479922C3"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C2555813-0DD3-46AA-B22F-3F0D479922C3.htm"
generated: "2025-11-28T19:06:34.145790Z"
description: Compares a layer state to the layers in the current drawing
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

# layerstate-compare (AutoLISP)

> Compares a layer state to the layers in the current drawing

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C2555813-0DD3-46AA-B22F-3F0D479922C3.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C2555813-0DD3-46AA-B22F-3F0D479922C3.htm)
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
(layerstate-compare
layerstatename viewport
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer state compare.
- ***viewport*:** **Type:**  Ename (entity name)  An ename of the viewport to be used in the compare. If viewport is `nil`, the current viewport is used

## Return Values

**Type:**  T or nil

`T`  if successful; otherwise `nil`.

## Examples

```lisp
(layerstate-compare "myLayerState")

nil
```
