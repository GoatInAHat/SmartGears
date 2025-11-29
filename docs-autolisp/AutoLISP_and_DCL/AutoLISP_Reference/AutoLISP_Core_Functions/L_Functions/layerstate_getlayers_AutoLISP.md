---
title: "layerstate-getlayers (AutoLISP)"
guid: "GUID-B0B12E74-D5EF-47E2-9B1C-ED595AA663A8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B0B12E74-D5EF-47E2-9B1C-ED595AA663A8.htm"
generated: "2025-11-28T19:06:34.522507Z"
description: Returns the layers saved in a layer state
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

# layerstate-getlayers (AutoLISP)

> Returns the layers saved in a layer state

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B0B12E74-D5EF-47E2-9B1C-ED595AA663A8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B0B12E74-D5EF-47E2-9B1C-ED595AA663A8.htm)
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
(layerstate-getlayers
layerstatename [invert]
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer state to query for layers.
- ***invert*:** **Type:**  List or nil  If invert is omitted or `nil`, returns a list of the layers saved in the layer state. If invert is `T`, it returns a list of the layers in the drawing that are not saved in the layer state.

## Return Values

**Type:**  List or nil

A list of layer names. Returns `nil`  if the layer state does not exist or contains no layers.

## Examples

```lisp
(layerstate-getlayers "myLayerState")

("Layername1" "Layername2")
```
