---
title: "layerstate-importfromdb (AutoLISP)"
guid: "GUID-A41D1A72-5DD2-469B-B0AD-97233A98D5BB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A41D1A72-5DD2-469B-B0AD-97233A98D5BB.htm"
generated: "2025-11-28T19:06:35.022155Z"
description: Imports a layer state from a specified drawing file
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

# layerstate-importfromdb (AutoLISP)

> Imports a layer state from a specified drawing file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A41D1A72-5DD2-469B-B0AD-97233A98D5BB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A41D1A72-5DD2-469B-B0AD-97233A98D5BB.htm)
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
(layerstate-importfromdb
layerstatename filename
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer state to be imported.
- ***filename*:** **Type:**  String  Name of the file from which to import a layer state.

## Return Values

**Type:**  T or nil

`T`  if the import is successful; `nil`  otherwise.

## Examples

- **Windows:** **(layerstate-importfromdb "mylayerstate" "c:\\mydrawing.dwg")**  T
- **Mac OS:** **(layerstate-importfromdb "mylayerstate" "/mydrawing.dwg")**  T
