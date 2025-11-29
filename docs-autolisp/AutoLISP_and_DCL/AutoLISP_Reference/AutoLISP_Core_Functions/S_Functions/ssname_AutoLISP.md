---
title: ssname (AutoLISP)
guid: "GUID-EFB83751-9AC5-4C52-AD3D-D971BC560C15"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EFB83751-9AC5-4C52-AD3D-D971BC560C15.htm"
generated: "2025-11-28T19:06:42.333038Z"
description: Returns the object (entity) name of the indexed element of a selection set
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

# ssname (AutoLISP)

> Returns the object (entity) name of the indexed element of a selection set

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EFB83751-9AC5-4C52-AD3D-D971BC560C15.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EFB83751-9AC5-4C52-AD3D-D971BC560C15.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(ssname
ss index
)
```

- ***ss*:** **Type:**  Pickset (selection set)  A selection set.
- ***index*:** **Type:**  Integer or Real  Element in a selection set. The first element in the set has an index of zero. To access entities beyond number 32,767 in a selection set, you must supply the *index*  argument as a real.

## Return Values

**Type:**  Ename (entity name) or nil

An entity name, if successful. If *index*  is negative or greater than the highest-numbered entity in the selection set, `ssname`  returns `nil`.

## Remarks

Entity names in selection sets obtained with `ssget`  are always names of main entities. Subentities (attributes and polyline vertices) are not returned. (The `entnext`  function allows access to them.)

## Examples

Get the name of the first entity in a selection set:

```lisp
(setq ent1 (ssname ss 0))

<Entity name: 1d62d68>
```

Get the name of the fourth entity in a selection set:

```lisp
(setq ent4 (ssname ss 3))

<Entity name: 1d62d90>
```

To access entities beyond the number 32,767 in a selection set, you must supply the *index*  argument as a real, as in the following example:

```lisp
(setq entx (ssname sset 50843.0))
```
