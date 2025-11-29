---
title: entupd (AutoLISP)
guid: "GUID-78313F48-676D-49D7-842A-288DD347A783"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-78313F48-676D-49D7-842A-288DD347A783.htm"
generated: "2025-11-28T19:06:28.574559Z"
description: Updates the screen image of an object (entity)
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 12/08/2024
topic_subtype:
  - autolisp
  - function
---

# entupd (AutoLISP)

> Updates the screen image of an object (entity)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-78313F48-676D-49D7-842A-288DD347A783.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-78313F48-676D-49D7-842A-288DD347A783.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 12/08/2024

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(entupd
ename
)
```

- ***ename*:** **Type:**  Ename (entity name)  The name of the entity to be updated on the screen.

## Return Values

**Type:**  Ename (entity name) or nil

The updated entity; otherwise `nil`, if nothing was updated.

## Remarks

When a "legacy" polyline vertex or block attribute is modified with `entmod`, the entire complex entity is not updated on the screen. The `entupd`  function can be used to cause a modified polyline or block to be updated on the screen. This function can be called with the entity name of any part of the polyline or block; it need not be the head entity. While `entupd`  is intended for polylines and blocks with attributes, it can be called for any entity. It always regenerates the entity on the screen, including all subentities.

Note:
 If
entupd
 is used on a nested entity (an entity within a block) or on a block that contains nested entities, some of the entities might not be regenerated. To ensure complete regeneration, you must invoke the AutoCAD REGEN command.

## Examples

Assuming that the first entity in the drawing is a 3D polyline with several vertices, the following code modifies and redisplays the polyline:

```lisp
(setq e1 (entnext))       ;
Sets
 e1
to the polyline's entity name

(setq e2 (entnext e1))    ;
Sets
 e2
to its first vertex

(setq ed (entget e2))     ;
Sets
 ed
to the vertex data

(setq ed
  (subst '(10 1.0 2.0)
    (assoc 10 ed)         ;
Changes the vertex's location in
 ed
    ed                    ;
to point (1,2)

  )
)
(entmod ed)               ;
Moves the vertex in the drawing

(entupd e1)               ;
Regenerates the polyline entity
 e1
```
