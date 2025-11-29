---
title: ssdel (AutoLISP)
guid: "GUID-F84DB8CE-B535-453D-977D-AE9D7AD2D7AA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F84DB8CE-B535-453D-977D-AE9D7AD2D7AA.htm"
generated: "2025-11-28T19:06:41.826806Z"
description: Deletes an object (entity) from a selection set
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

# ssdel (AutoLISP)

> Deletes an object (entity) from a selection set

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F84DB8CE-B535-453D-977D-AE9D7AD2D7AA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F84DB8CE-B535-453D-977D-AE9D7AD2D7AA.htm)
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
(ssdel
ename ss
)
```

- ***ename*:** **Type:**  Ename (entity name)  An entity name.
- ***ss*:** **Type:**  Pickset (selection set)  A selection set.

## Return Values

**Type:**  Pickset (selection set) or nil

The name of the selection set; otherwise `nil`, if the specified entity is not in the set.

Note that the entity is actually deleted from the existing selection set, as opposed to a new set being returned with the element deleted.

## Examples

In the following examples, entity name `e1`  is a member of selection set `ss`, while entity name `e3`  is not a member of `ss`:

```lisp
(ssdel e1 ss)

<Selection set: 2>
```

Selection set `ss`  is returned with entity `e1`  removed.

```lisp
(ssdel e3 ss)

nil
```

The function returns `nil`  because `e3`  is not a member of selection set `ss`.
