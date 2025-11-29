---
title: ssadd (AutoLISP)
guid: "GUID-22337977-82F2-4394-B209-39DE2B4B9E86"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-22337977-82F2-4394-B209-39DE2B4B9E86.htm"
generated: "2025-11-28T19:06:41.749981Z"
description: Adds an object (entity) to a selection set, or creates a new selection set
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

# ssadd (AutoLISP)

> Adds an object (entity) to a selection set, or creates a new selection set

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-22337977-82F2-4394-B209-39DE2B4B9E86.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-22337977-82F2-4394-B209-39DE2B4B9E86.htm)
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
(ssadd
[ename [ss]]
)
```

- ***ename*:** **Type:**  Ename (entity name)  An entity name.
- ***ss*:** **Type:**  Pickset (selection set)  A selection set.

## Return Values

**Type:**  Pickset (selection set) or nil

The modified selection set passed as the second argument, if successful; otherwise `nil`.

## Remarks

If called with no arguments, `ssadd`  constructs a new selection set with no members. If called with the single entity name argument *ename*, `ssadd`  constructs a new selection set containing that single entity. If called with an entity name and the selection set *ss*, `ssadd`  adds the named entity to the selection set.

## Examples

When adding an entity to a set, the new entity is added to the existing set, and the set passed as *ss*  is returned as the result. Thus, if the set is assigned to other variables, they also reflect the addition. If the named entity is already in the set, the `ssadd`  operation is ignored and no error is reported.

Set `e1`  to the name of the first entity in drawing:

```lisp
(setq e1 (entnext))

<Entity name: 1d62d60>
```

Set `ss`  to a null selection set:

```lisp
(setq ss (ssadd))

<Selection set: 2>
```

The following command adds the `e1`  entity to the selection set referenced by `ss`:

```lisp
(ssadd e1 ss)

<Selection set: 2>
```

Get the entity following `e1`:

```lisp
(setq e2 (entnext e1))

<Entity name: 1d62d68>
```

Add `e2`  to the `ss`  entity:

```lisp
(ssadd e2 ss)

<Selection set: 2>
```
