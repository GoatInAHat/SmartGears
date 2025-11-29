---
title: entdel (AutoLISP)
guid: "GUID-AF320BD5-C83C-4EA0-983E-1EC885F5FC70"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AF320BD5-C83C-4EA0-983E-1EC885F5FC70.htm"
generated: "2025-11-28T19:06:27.763723Z"
description: Deletes objects (entities) or restores previously deleted objects
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

# entdel (AutoLISP)

> Deletes objects (entities) or restores previously deleted objects

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AF320BD5-C83C-4EA0-983E-1EC885F5FC70.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AF320BD5-C83C-4EA0-983E-1EC885F5FC70.htm)
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
(entdel
ename
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the entity to be deleted or restored.

## Return Values

**Type:**  Ename (entity name)

The entity name.

## Remarks

The entity specified by *ename*  is deleted if it is currently in the drawing. The `entdel`  function restores the entity to the drawing if it has been deleted previously in this editing session. Deleted entities are purged from the drawing when the drawing is exited. The `entdel`  function can delete both graphical and nongraphical entities.

The `entdel`  function operates only on main entities. Attributes and polyline vertices cannot be deleted independently of their parent entities. You can use the `command`  function to operate the AutoCAD ATTEDIT or PEDIT commands to modify subentities.

You cannot delete entities within a block definition. However, you can completely redefine a block definition, minus the entity you want deleted, with `entmake`.

## Examples

Get the name of the first entity in the drawing and assign it to variable `e1`:

```lisp
(setq e1 (entnext))

<Entity name: 2c90520>
```

Delete the entity named by e1:

```lisp
(entdel e1)

<Entity name: 2c90520>
```

Restore the entity named by e1:

```lisp
(entdel e1)
 <Entity name: 2c90520>
```
