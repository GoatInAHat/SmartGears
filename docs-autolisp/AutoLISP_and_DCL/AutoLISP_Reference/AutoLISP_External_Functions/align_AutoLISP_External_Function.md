---
title: align (AutoLISP/External Function)
guid: "GUID-9A7D9FFE-23F8-459D-878A-A6F4CD4368AC"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9A7D9FFE-23F8-459D-878A-A6F4CD4368AC.htm"
generated: "2025-11-28T19:06:53.319731Z"
description: Translates and rotates objects, allowing them to be aligned with other objects
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

# align (AutoLISP/External Function)

> Translates and rotates objects, allowing them to be aligned with other objects

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9A7D9FFE-23F8-459D-878A-A6F4CD4368AC.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9A7D9FFE-23F8-459D-878A-A6F4CD4368AC.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows and Mac OS only

**Prerequisites:** The Geom3d ObjectARX application must be loaded before the function can be called, `(arxload "geom3d")`.

## Signature

```lisp
(align
args ...
)
```

- ***args*:** **Type:**  String, List, Ename (entity name), or nil  The order, number, and type of arguments for the `align`  function are the same as if you were using the AutoCAD ALIGN command.  A null response (a user pressing Enter) can be indicated by specifying `nil`  or an empty string (`""`).

## Return Values

**Type:**  T or nil

If successful, `align`  returns `T`; otherwise it returns `nil`.

## Examples

The following example specifies two pairs of source and destination points, which perform a 2D move and does not scale the objects based on alignment points:

```lisp
(setq ss (ssget))
(setq s1 (getpoint "\nSource1: "))
(setq d1 (getpoint s1 "\nDestination1: "))
(setq s2 (getpoint "\nSource2: "))
(setq d2 (getpoint s2 "\nDestination2: "))
(align ss s1 d1 s2 d2 "" "n")

T
```
