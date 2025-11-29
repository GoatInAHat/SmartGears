---
title: entget (AutoLISP)
guid: "GUID-12540DAE-C84B-4BDB-AEEC-DDFE5BE3C42A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-12540DAE-C84B-4BDB-AEEC-DDFE5BE3C42A.htm"
generated: "2025-11-28T19:06:27.854051Z"
description: Retrieves an object's (entity's) definition data
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

# entget (AutoLISP)

> Retrieves an object's (entity's) definition data

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-12540DAE-C84B-4BDB-AEEC-DDFE5BE3C42A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-12540DAE-C84B-4BDB-AEEC-DDFE5BE3C42A.htm)
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
(entget
ename [applist]
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the entity being queried. The *ename*  can refer to either a graphical or a nongraphical entity.
- ***applist*:** **Type:**  List  A list of registered application names.

## Return Values

**Type:**  Ename (entity name)

An association list containing the entity definition of *ename*. If you specify the optional *applist*  argument, `entget`  also returns the extended data associated with the specified applications. Objects in the list are assigned AutoCAD DXF™ group codes for each part of the entity data.

Note that the DXF group codes used by AutoLISP differ slightly from the group codes in a DXF file.

## Examples

Assume that the last object created in the drawing is a line drawn from point (1,2) to point (6,5). The following example shows code that retrieves the entity name of the last object with the `entlast`  function, and passes that name to `entget`:

```lisp
(entget (entlast))

((-1 . <Entity name: 1bbd1d0>) (0 . "LINE") (330 . <Entity name: 1bbd0c8>)
(5 . "6A") (100 . "AcDbEntity") (67 . 0) (410 . "Model") (8 . "0") (100 . "AcDbLine")
(10 1.0 2.0 0.0) (11 6.0 5.0 0.0) (210 0.0 0.0 1.0))
```
