---
title: ispropertyreadonly (AutoLISP)
guid: "GUID-1DE8095D-5755-4889-BFB7-C13045B7BC81"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1DE8095D-5755-4889-BFB7-C13045B7BC81.htm"
generated: "2025-11-28T19:06:33.483625Z"
description: "Returns the read-only state of an entity’s property"
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

# ispropertyreadonly (AutoLISP)

> Returns the read-only state of an entity’s property

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1DE8095D-5755-4889-BFB7-C13045B7BC81.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1DE8095D-5755-4889-BFB7-C13045B7BC81.htm)
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
(ispropertyreadonly
ename propertyname [or collectionName index name]
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the entity being queried. The *ename*  can refer to either a graphical or a non-graphical entity.
- ***propertyname*:** **Type:**  String  Name of the property being queried. For a list of all the valid property names of a given object, use `dumpallproperties`.
- ***collectionName*:** **Type:**  String  If the object is a collection object, the Collection name is passed here.
- ***index*:** **Type:**  Integer  The collection index being queried.
- ***name*:** **Type:**  String  The name of the property within the collection being queried.

## Return Values

**Type:**  Integer

1 is returned when the property is read-only; otherwise, 0 is returned when the property is writable.

## Examples

The following example demonstrates how to check the read-only state of the Radius and Area properties of a circle.

```lisp
(setq e1 (car (entsel "\nSelect an arc or circle: ")))

<Entity name: 10e2e4ba0>

(ispropertyreadonly e1 "Radius")

0

(ispropertyreadonly e1 "Area")

1
```
