---
title: "layerstate-removelayers (AutoLISP)"
guid: "GUID-5F9FFCEB-D7D6-4233-AC74-526B51C4FD42"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5F9FFCEB-D7D6-4233-AC74-526B51C4FD42.htm"
generated: "2025-11-28T19:06:35.190558Z"
description: Removes a list of layers from a layer state
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

# layerstate-removelayers (AutoLISP)

> Removes a list of layers from a layer state

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5F9FFCEB-D7D6-4233-AC74-526B51C4FD42.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5F9FFCEB-D7D6-4233-AC74-526B51C4FD42.htm)
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
(layerstate-removelayers
layerstatename (list layername layername layername ...)
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer state to be updated.
- ***layername*:** **Type:**  String  Name of the layer state to be removed.

## Return Values

**Type:**  T or nil

`T`  if the remove is successful; otherwise `nil`.

## Examples

```lisp
(layerstate-removelayers "myLayerState" (list "Walls" "Elec1" "Foundation" "Plumbing"))

T
```
