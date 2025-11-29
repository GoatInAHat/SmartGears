---
title: entnext (AutoLISP)
guid: "GUID-65924CF5-0C51-4E36-8B38-7A5513951A04"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-65924CF5-0C51-4E36-8B38-7A5513951A04.htm"
generated: "2025-11-28T19:06:28.401508Z"
description: Returns the name of the next object (entity) in the drawing
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

# entnext (AutoLISP)

> Returns the name of the next object (entity) in the drawing

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-65924CF5-0C51-4E36-8B38-7A5513951A04.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-65924CF5-0C51-4E36-8B38-7A5513951A04.htm)
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
(entnext
[ename]
)
```

- ***ename*:** **Type:**  Ename (entity name)  The name of an existing entity.

## Return Values

**Type:**  Ename (entity name) or nil

If `entnext`  is called with no arguments, it returns the entity name of the first nondeleted entity in the database. If an *ename*  argument is supplied to `entnext`, the function returns the entity name of the first nondeleted entity following *ename*  in the database. If there is no next entity in the database, it returns `nil`. The `entnext`  function returns both main entities and subentities.

## Examples

```lisp
(setq e1 (entnext))    ;
Sets
 e1
to the name of the first entity in  the  drawing

(setq e2 (entnext e1)) ;
Sets
 e2
to the name of the entity following
 e1
```

Note:
 The entities selected by
ssget
 are main entities, not attributes of blocks or vertices of polylines. You can access the internal structure of these complex entities by walking through the subentities with
entnext
. Once you obtain a subentity's name, you can operate on it like any other entity. If you obtain the name of a subentity with
entnext
, you can find the parent entity by stepping forward with
entnext
 until a seqend entity is found, then extracting the -2 group from that entity, which is the main entity's name.
