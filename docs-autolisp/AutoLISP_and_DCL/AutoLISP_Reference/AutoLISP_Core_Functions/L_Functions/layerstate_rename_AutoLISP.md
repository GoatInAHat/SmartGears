---
title: "layerstate-rename (AutoLISP)"
guid: "GUID-843B8FC9-A1F5-48CE-B83D-98D747E85C83"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-843B8FC9-A1F5-48CE-B83D-98D747E85C83.htm"
generated: "2025-11-28T19:06:35.271909Z"
description: Renames a layer state
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

# layerstate-rename (AutoLISP)

> Renames a layer state

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-843B8FC9-A1F5-48CE-B83D-98D747E85C83.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-843B8FC9-A1F5-48CE-B83D-98D747E85C83.htm)
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
(layerstate-rename
oldlayerstatename newlayerstatename
)
```

- ***oldlayerstatename*:** **Type:**  String  Name of the layer state to be renamed.
- ***newlayerstatename*:** **Type:**  String  Name of the layer state to be updated.

## Return Values

**Type:**  T or nil

`T`  if the rename is successful; otherwise `nil`.

## Examples

```lisp
(layerstate-rename "myLayerState" "myNewLayerState")

T
```
