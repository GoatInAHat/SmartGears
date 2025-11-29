---
title: entlast (AutoLISP)
guid: "GUID-75DBA9B2-034B-4377-A4E2-21D37B298D86"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-75DBA9B2-034B-4377-A4E2-21D37B298D86.htm"
generated: "2025-11-28T19:06:27.961853Z"
description: Returns the name of the last nondeleted main object (entity) in the drawing
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

# entlast (AutoLISP)

> Returns the name of the last nondeleted main object (entity) in the drawing

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-75DBA9B2-034B-4377-A4E2-21D37B298D86.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-75DBA9B2-034B-4377-A4E2-21D37B298D86.htm)
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
(entlast)
```

 No arguments.

## Return Values

**Type:**  Ename (entity name)

An entity name; otherwise `nil`, if there are no entities in the current drawing.

## Remarks

The `entlast`  function is frequently used to obtain the name of a new entity that has just been added with the `command`  function. To be selected, the entity need not be on the screen or on a thawed layer.

## Examples

Set variable `e1`  to the name of the last entity added to the drawing:

```lisp
(setq e1 (entlast))

<Entity name: 2c90538>
```

If your application requires the name of the last nondeleted entity (main entity or subentity), define a function such as the following and call it instead of `entlast`.

```lisp
(defun lastent (/ a b)
  (if (setq a (entlast))
Gets last main entity

    (while (setq b (entnext a))
If subentities follow, loops
until there are no more

      (setq a b)
subentities

    )
  )
  a
Returns last main entity

)
or subentity
```
