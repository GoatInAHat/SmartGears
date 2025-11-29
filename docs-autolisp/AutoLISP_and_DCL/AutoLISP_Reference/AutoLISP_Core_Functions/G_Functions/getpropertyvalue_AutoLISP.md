---
title: getpropertyvalue (AutoLISP)
guid: "GUID-8E5913FC-09ED-4C70-AFB7-2431C062E899"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8E5913FC-09ED-4C70-AFB7-2431C062E899.htm"
generated: "2025-11-28T19:06:31.438640Z"
description: Returns the current value of an entity’s property
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

# getpropertyvalue (AutoLISP)

> Returns the current value of an entity’s property

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8E5913FC-09ED-4C70-AFB7-2431C062E899.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8E5913FC-09ED-4C70-AFB7-2431C062E899.htm)
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
(getpropertyvalue
ename propertyname [or collectionName index name]
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the entity being queried. The *ename*  can refer to either a graphical or a nongraphical entity.
- ***propertyname*:** **Type:**  String  Name of the property being queried. For a list of all the valid property names of a given object, use `dumpallproperties`.
- ***collectionName*:** **Type:**  String  If the object is a collection object, the Collection name is passed here.
- ***index*:** **Type:**  Integer  The collection index being queried.
- ***name*:** **Type:**  String  The name of the property within the collection being queried.

## Return Values

**Type:**  Integer, Real, String, List, T, or nil

The value of the entity’s property.

## Examples

The following example demonstrates how to get the current radius value of a circle.

```lisp
(command "_circle" "2,2" 2)

nil

(getpropertyvalue (entlast) "radius")

2.0
```
