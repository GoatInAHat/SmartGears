---
title: 3dsin (AutoLISP/External Function)
guid: "GUID-CB6540BA-B16A-4F3D-8920-F4D2D714A6B5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CB6540BA-B16A-4F3D-8920-F4D2D714A6B5.htm"
generated: "2025-11-28T19:06:53.209764Z"
description: Imports a 3D Studio (.3ds) file
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

# 3dsin (AutoLISP/External Function)

> Imports a 3D Studio ( .3ds ) file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CB6540BA-B16A-4F3D-8920-F4D2D714A6B5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CB6540BA-B16A-4F3D-8920-F4D2D714A6B5.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows only; not available in AutoCAD LT for Windows

**Prerequisites:** The AcRender ObjectARX application must be loaded before the function can be called, `(arxload "acrender")`. Earlier releases might require you to load the *render.arx*  file.

## Signature

```lisp
(c:3dsin
mode [multimat create] file
)
```

- **mode:** **Type:**  Integer  A numeric value that specifies whether the command is to be used interactively (mode = 1) or non-interactively (mode = 0)
- **multimat:** **Type:**  Integer  A numeric value that specifies how to treat objects with multiple materials. Required if *mode*  is set to 0. Allowable values are  **0**  Create a new object for each material  **1**  Assign the first material to the new object
- **create:** **Type:**  Integer  A numeric value that specifies how to organize new objects. This mode always imports all the objects in the *.3ds* file. Required if *mode*  is set to 0. Allowable values are  **0**  Create a layer for each 3DS object  **1**  Create a layer for each 3DS color  **2**  Create a layer for each 3DS material  **3**  Place all new objects on a single layer
- **file:** **Type:**  String  3DS file to import; the *.3ds*  file extension is required.

## Return Values

**Type:**  Integer or nil

A numeric value if the file was successfully imported; otherwise, `nil`  is returned if the file could not be imported.

## Examples

Import all of *shadow.3ds*  with no user input, splitting objects with multiple materials and putting all new objects on the same later:

```lisp
(c:3dsin 0 0 3 "c:/my documents/cad drawings/shadow.3ds")

Initializing Render...
Initializing preferences...done.
Processing object B_Leg01
Converting material SKIN
Processing object B_Leg02
Processing object Central_01
Processing object Central_02
Processing object F_Leg01
Processing object F_Leg02
Processing object M_Quad01
Processing object ML_Feele01
Processing object ML_Feele02
Processing object Pre_Quad01
Processing object Pre_Quad02
3D Studio file import completed
1
```
