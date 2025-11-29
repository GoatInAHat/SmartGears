---
title: setpropertyvalue (AutoLISP)
guid: "GUID-8F32FD8C-D81A-4DCA-B455-9D560CF17246"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8F32FD8C-D81A-4DCA-B455-9D560CF17246.htm"
generated: "2025-11-28T19:06:40.725586Z"
description: Sets the property value for an entity
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

# setpropertyvalue (AutoLISP)

> Sets the property value for an entity

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8F32FD8C-D81A-4DCA-B455-9D560CF17246.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8F32FD8C-D81A-4DCA-B455-9D560CF17246.htm)
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
(setpropertyvalue
ename propertyname value [or collectionName index name val]
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the entity being modified. The *ename*  can refer to either a graphical or a non-graphical entity.
- ***propertyname*:** **Type:**  String  Name of the property to be modified. For a list of all the valid property names of a given object, use `dumpallproperties`.
- ***value*:** **Type:**  Integer, Real, String, List, T, or nil  Value to set the property to when the object is not a collection.
- ***collectionName*:** **Type:**  String  If the object is a collection object, the Collection name is passed here.
- ***index*:** **Type:**  Integer  The collection index to be modified.
- ***name*:** **Type:**  String  Name of the property in the collection to be modified.
- ***val*:** **Type:**  Integer, Real, String, List, T, or nil  Value to set the property to.

## Return Values

**Type:**  nil

`nil`  is returned unless an error occurs when the property value is being updated.

## Examples

The following example demonstrates how to change the radius of a circle.

```lisp
(command "._circle" "2,2" 2)

nil

(setpropertyvalue (entlast) "radius" 3)

nil
```

The following example demonstrates how to apply overrides to a linear dimension.

```lisp
(command "._dimlinear" "2,2" "5,4" "3,3")

nil

(setq e2 (entlast))

<Entity name: 10e2e4bd0>

(setpropertyvalue e2 "Dimtfill" 2)

nil

(setpropertyvalue e2 "Dimtfillclr" "2")

nil

(setpropertyvalue e2 "Dimclrt" "255,0,0")

nil
```

The following example demonstrates how to change the first vertex of the Vertices collection.

```lisp
(command "._pline" "0,0" "3,3" "5,2" "")

nil

(setq e3 (entlast))

<Entity name: 10e2e4da0>

(setpropertyvalue e3 "Vertices" 0 "EndWidth" 1.0)

nil
```
