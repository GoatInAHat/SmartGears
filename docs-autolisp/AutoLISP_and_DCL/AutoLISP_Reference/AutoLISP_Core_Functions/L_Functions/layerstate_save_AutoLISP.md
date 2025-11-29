---
title: "layerstate-save (AutoLISP)"
guid: "GUID-05B5EA5C-2E7F-47A5-9C6F-27902DB60F30"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-05B5EA5C-2E7F-47A5-9C6F-27902DB60F30.htm"
generated: "2025-11-28T19:06:35.491380Z"
description: Saves a layer state in the current drawing
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

# layerstate-save (AutoLISP)

> Saves a layer state in the current drawing

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-05B5EA5C-2E7F-47A5-9C6F-27902DB60F30.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-05B5EA5C-2E7F-47A5-9C6F-27902DB60F30.htm)
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
(layerstate-save
layerstatename mask viewport
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer state to save.
- ***mask*:** **Type:**  Integer  Numeric sum designating which properties in the layer state are to be restored.  **1**  -- Restore the saved On or Off value  **2**  -- Restore the saved Frozen or Thawed value  **4**  -- Restore the saved Lock value  **8**  -- Restore the saved Plot or No Plot value  **16**  -- Restore the saved VPVSDFLT value  **32**  -- Restore the saved Color  **64**  -- Restore the saved LineType  **128**  -- Restore the saved LineWeight
- ***viewport*:** **Type:**  Ename (entity name)  An ename of the viewport whose VPLAYER setting is to be captured. If nil, the layer state will be saved without VPLAYER settings.

## Return Values

**Type:**  T or nil

`T`  if the save is successful; otherwise `nil`.

## Examples

```lisp
(layerstate-save "myLayerState" 21 viewportId)

T

(layerstate-save "myLayerState" nil nil)

nil
```
