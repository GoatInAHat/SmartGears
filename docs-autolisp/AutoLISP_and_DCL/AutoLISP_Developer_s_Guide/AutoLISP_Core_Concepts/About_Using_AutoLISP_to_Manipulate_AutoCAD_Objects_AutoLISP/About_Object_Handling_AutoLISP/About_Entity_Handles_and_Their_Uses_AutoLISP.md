---
title: About Entity Handles and Their Uses (AutoLISP)
guid: "GUID-30C33AF6-4BE3-4334-96BD-F929040C31D3"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-30C33AF6-4BE3-4334-96BD-F929040C31D3.htm"
generated: "2025-11-28T19:06:13.472808Z"
description: The handent function retrieves the name of an entity with a specific handle.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# About Entity Handles and Their Uses (AutoLISP)

> The handent function retrieves the name of an entity with a specific handle.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-30C33AF6-4BE3-4334-96BD-F929040C31D3.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-30C33AF6-4BE3-4334-96BD-F929040C31D3.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

As with entity names, handles are unique within a drawing. However, an entity's handle is constant throughout its life. AutoLISP applications that manipulate a specific database can use `handent`  to obtain the current name of an entity they must use. You can use the AutoCAD LIST command to get the handle of a selected object.

The following example code uses `handent`  to obtain and display the entity name that is associated with the handle “5a2”.

```lisp
(if (not (setq e1 (handent "5a2")))
  (princ "\nNo entity with that handle exists. ")
  (princ e1)
)
```

In one particular editing session, this code might display the following:

```lisp
<Entity name: 60004722>
```

In another editing session with the same drawing, the fragment might display an entirely different number. But in both cases the code would be accessing the same entity.

The `handent`  function has an additional use. Entities can be deleted from the database with `entdel`. The entities are not purged until the current drawing ends. This means that `handent`  can recover the names of deleted entities, which can then be restored to the drawing by a second call to `entdel`.

Note:
 Handles are provided for block definitions, including subentities.

Entities in drawings that are cross-referenced by way of XREF Attach are not actually part of the current drawing; their handles are unchanged but cannot be accessed by `handent`. However, when drawings are combined by means of INSERT, INSERT *, XREF Bind (XBIND), or partial DXFIN, the handles of entities in the incoming drawing are lost, and incoming entities are assigned new handle values to ensure each handle in the current drawing remains unique.
