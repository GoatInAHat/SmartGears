---
title: "layerstate-delete (AutoLISP)"
guid: "GUID-828C4B51-7C59-4FF0-AA69-907C53F31703"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-828C4B51-7C59-4FF0-AA69-907C53F31703.htm"
generated: "2025-11-28T19:06:34.263801Z"
description: Deletes a layer state
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

# layerstate-delete (AutoLISP)

> Deletes a layer state

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-828C4B51-7C59-4FF0-AA69-907C53F31703.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-828C4B51-7C59-4FF0-AA69-907C53F31703.htm)
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
(layerstate-delete
layerstatename
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer state to be deleted.

## Return Values

**Type:**  T or nil

`T`  if the delete succeeds; otherwise `nil`.

## Examples

```lisp
(layerstate-delete "myLayerState")

T
```
