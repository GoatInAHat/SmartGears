---
title: "layerstate-import (AutoLISP)"
guid: "GUID-E8B76E26-4E16-4AC6-B5E1-2CEA0C21EA1B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E8B76E26-4E16-4AC6-B5E1-2CEA0C21EA1B.htm"
generated: "2025-11-28T19:06:34.918122Z"
description: Imports a layer state from a specified file
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

# layerstate-import (AutoLISP)

> Imports a layer state from a specified file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E8B76E26-4E16-4AC6-B5E1-2CEA0C21EA1B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E8B76E26-4E16-4AC6-B5E1-2CEA0C21EA1B.htm)
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
(layerstate-import
filename
)
```

- ***filename*:** **Type:**  String  Name of the file from which to import a layer state.

## Return Values

**Type:**  T or nil

`T`  if the import is successful; `nil`  otherwise.

## Examples

- **Windows:** **(layerstate-import "c:\\mylayerstate.las")**  T
- **Mac OS:** **(layerstate-import "/mylayerstate.las")**  T
