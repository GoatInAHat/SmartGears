---
title: rotate3d (AutoLISP/External Function)
guid: "GUID-95ECADE6-B791-4B9A-8FAE-CD981F803689"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-95ECADE6-B791-4B9A-8FAE-CD981F803689.htm"
generated: "2025-11-28T19:06:53.625808Z"
description: Rotates an object about an arbitrary 3D axis
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

# rotate3d (AutoLISP/External Function)

> Rotates an object about an arbitrary 3D axis

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-95ECADE6-B791-4B9A-8FAE-CD981F803689.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-95ECADE6-B791-4B9A-8FAE-CD981F803689.htm)
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
(rotate3d
args ...
)
```

- ***args*:** **Type:**  String, List, Ename (entity name), or nil  The order, number, and type of arguments for the `rotate3d`  function are the same as if you were using the AutoCAD ROTATE3D command.  A null response (a user pressing Enter) can be indicated by specifying `nil`  or an empty string (`""`).

## Return Values

**Type:**  T or nil

If successful, `rotate3d`  returns `T`; otherwise it returns `nil`.

## Examples

The following example rotates the selected objects 30 degrees about the axis specified by points *p1*  and *p2*.

```lisp
(setq ss (ssget))
(setq p1 (getpoint "\nPoint1: "))
(setq p2 (getpoint "\nPoint2: "))

(rotate3d ss p1 p2 30)
```

AutoLISP support for the `rotate3d`  function is implemented with the use of the SAGET library.
