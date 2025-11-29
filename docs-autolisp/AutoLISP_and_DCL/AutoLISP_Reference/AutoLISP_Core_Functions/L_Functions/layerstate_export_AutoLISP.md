---
title: "layerstate-export (AutoLISP)"
guid: "GUID-E14DB236-6DD9-48A8-91F3-8A44A6E8BAA4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E14DB236-6DD9-48A8-91F3-8A44A6E8BAA4.htm"
generated: "2025-11-28T19:06:34.354075Z"
description: Exports a layer state to a specified file
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

# layerstate-export (AutoLISP)

> Exports a layer state to a specified file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E14DB236-6DD9-48A8-91F3-8A44A6E8BAA4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E14DB236-6DD9-48A8-91F3-8A44A6E8BAA4.htm)
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
(layerstate-export
layerstatename filename
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer to export.
- ***filename*:** **Type:**  String  Name of the file to which the layer state should be exported.

## Return Values

**Type:**  T or nil

`T`  if the export is successful; `nil`  otherwise.

## Examples

- **Windows:** **(layerstate-export "myLayerState" "c:\\mylayerstate.las")**  T
- **Mac OS:** **(layerstate-export "myLayerState" "/mylayerstate.las")**  T
