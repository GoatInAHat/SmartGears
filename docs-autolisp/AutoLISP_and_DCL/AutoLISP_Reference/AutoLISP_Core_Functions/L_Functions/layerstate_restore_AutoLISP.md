---
title: "layerstate-restore (AutoLISP)"
guid: "GUID-84C7195D-12C1-4282-B5EE-8B9D6DDB705A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-84C7195D-12C1-4282-B5EE-8B9D6DDB705A.htm"
generated: "2025-11-28T19:06:35.411052Z"
description: Restores a layer state into the current drawing
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

# layerstate-restore (AutoLISP)

> Restores a layer state into the current drawing

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-84C7195D-12C1-4282-B5EE-8B9D6DDB705A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-84C7195D-12C1-4282-B5EE-8B9D6DDB705A.htm)
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
(layerstate-restore
layerstatename viewport [restoreflags]
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer to restore.
- **viewport:** **Type:**  Ename (entity name)  An ename of the viewport to which *layerstatename*  should be restored. If viewport is `nil`, the layer state is restored to model space.
- **restoreflags:** **Type:**  Integer  Optional numeric sum affecting how the layer state is restored.  **1**  -- Turn off all layers not in the restored layer state  **2**  -- Freeze all layers not in the restored layer state  **4**  -- Restore the layer state properties as viewport overrides (requires viewport to be not a `nil`  value).

## Return Values

**Type:**  List or nil

`nil`  if the layer state does not exist or contains no layers; otherwise, returns a list of layer names.

## Examples

```lisp
(layerstate-restore "myLayerState" viewportId 5)

("Layername1" "Layername2")
```
