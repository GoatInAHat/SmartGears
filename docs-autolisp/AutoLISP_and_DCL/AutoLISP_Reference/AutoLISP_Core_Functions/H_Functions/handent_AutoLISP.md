---
title: handent (AutoLISP)
guid: "GUID-AF6DD533-1A24-4687-96EB-F03F26050C07"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AF6DD533-1A24-4687-96EB-F03F26050C07.htm"
generated: "2025-11-28T19:06:32.555907Z"
description: Returns an object (entity) name based on its handle
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

# handent (AutoLISP)

> Returns an object (entity) name based on its handle

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AF6DD533-1A24-4687-96EB-F03F26050C07.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AF6DD533-1A24-4687-96EB-F03F26050C07.htm)
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
(handent
handle
)
```

- ***handle*:** **Type:**  String  A string identifying an entity handle.

## Return Values

**Type:**  Ename (entity name) or nil

If successful, `handent`  returns the entity name associated with *handle*  in the current editing session. If `handent`  is passed an invalid handle or a handle not used by any entity in the current drawing, it returns `nil`.

The `handent`  function returns entities that have been deleted during the current editing session. You can undelete them with the `entdel`  function.

An entity's name can change from one editing session to the next, but an entity's handle remains constant.

## Remarks

The `handent`  function returns the entity name of both graphic and nongraphical entities.

## Examples

```lisp
(handent "5A2")

<Entity name: 60004722>
```

Used with the same drawing but in another editing session, the same call might return a different entity name. Once the entity name is obtained, you can use it to manipulate the entity with any of the entity-related functions.
