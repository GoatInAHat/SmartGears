---
title: mirror3d (AutoLISP/External Function)
guid: "GUID-43CA1888-1542-466B-AF84-66204078E99C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-43CA1888-1542-466B-AF84-66204078E99C.htm"
generated: "2025-11-28T19:06:53.512088Z"
description: "Reflects selected objects about a user-specified plane"
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

# mirror3d (AutoLISP/External Function)

> Reflects selected objects about a user-specified plane

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-43CA1888-1542-466B-AF84-66204078E99C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-43CA1888-1542-466B-AF84-66204078E99C.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows and Mac OS only; not available in AutoCAD LT

**Prerequisites:** The Geom3d ObjectARX application must be loaded before the function can be called, `(arxload "geom3d")`.

## Signature

```lisp
(mirror3d
args ...
)
```

- ***args*:** **Type:**  String, List, Ename (entity name), or nil  The order, number, and type of arguments for the `mirror3d`  function are the same as if you were using the AutoCAD MIRROR3D command.  A null response (a user pressing Enter) can be indicated by specifying `nil`  or an empty string (`""`).

## Return Values

**Type:**  T or nil

If successful, `mirror3d`  returns `T`; otherwise it returns `nil`.

## Examples

The following example mirrors the selected objects about the *XY*  plane that passes through the point 0,0,5, and then deletes the old objects:

```lisp
(setq ss (ssget))
(mirror3d ss "XY" '(0 0 5) "Y")

T
```
