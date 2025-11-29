---
title: "layerstate-addlayers (AutoLISP)"
guid: "GUID-F392F4F2-B2C1-4C8E-AEBB-1D3FE31610C9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F392F4F2-B2C1-4C8E-AEBB-1D3FE31610C9.htm"
generated: "2025-11-28T19:06:34.013717Z"
description: Adds or updates a series of layers to a layer state
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

# layerstate-addlayers (AutoLISP)

> Adds or updates a series of layers to a layer state

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F392F4F2-B2C1-4C8E-AEBB-1D3FE31610C9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F392F4F2-B2C1-4C8E-AEBB-1D3FE31610C9.htm)
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
(layerstate-addlayers
layerstatename (list layername state color linetype lineweight plotstyle) [(list ...)]
)
```

- ***layerstatename*:** **Type:**  String  Name of the layer state to be updated.
- ***layername*:** **Type:**  String  Name of the layer to be added or updated.
- ***state*:** **Type:**  Integer or nil  Numeric sum designating properties in the layer to be set.  **1**  -- Turns the layer off  **2**  -- Freeze the layer  **4**  -- Lock the layer  **8**  -- Flag the layer as No Plot  **16**  -- Set the layer as being frozen in new viewports  A `nil`  value uses defaults of on, thawed, unlocked, plottable, and thawed in new viewports.
- ***color*:** **Type:**  List  A dotted pair specifying the layers color type and value, *e.g.*  `(62 . ColorIndex)`, `(420 . TrueColor)`, or `(430 . "colorbook$colorname")`.
- ***linetype*:** **Type:**  String  Name of the layer linetype. The linetype must already be loaded in the drawing or the default of "Continuous" will be used. A `nil`  value sets the layer linetype to "Continuous."
- ***lineweight*:** **Type:**  Integer  Number corresponding to a valid lineweight, i.e., 35 = .35, 211 = 2.11. A `nil`  value sets the layer lineweight to "Default."
- ***plotstyle*:** **Type:**  String  Name of the layers plot style. The plotstyle name must already be loaded in the drawing or the default of "Normal" will be used. A `nil`  value sets the layer plotstyle to "Normal." If the drawing is in color dependent mode, this setting is ignored.

## Return Values

**Type:**  T or nil

`T`  if successful; otherwise `nil`.

## Examples

```lisp
(layerstate-addlayers
  "myLayerState"
  (list "Walls" 4 '(62 . 45) "Divide" 35 "10% Screen")
  (list "Floors" 6 '(420 . 16235019) "Continuous" 40 "60% Screen")
  (list "Ceiling" 0 '(430 . "RAL CLASSIC$RAL 1003") "DOT" nil nil)
)

T
```
