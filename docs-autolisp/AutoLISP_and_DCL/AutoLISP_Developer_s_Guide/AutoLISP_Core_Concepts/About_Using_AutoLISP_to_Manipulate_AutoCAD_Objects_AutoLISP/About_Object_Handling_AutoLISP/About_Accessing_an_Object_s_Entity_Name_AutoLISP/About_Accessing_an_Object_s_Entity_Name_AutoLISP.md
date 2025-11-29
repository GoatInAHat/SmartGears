---
title: About Accessing an Object’s Entity Name (AutoLISP)
guid: "GUID-3EEA3D4B-36B6-4E3B-B9E8-62B6265FA68C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3EEA3D4B-36B6-4E3B-B9E8-62B6265FA68C.htm"
generated: "2025-11-28T19:06:11.718534Z"
description: An AutoLISP routine must obtain an object’s entity name to make subsequent calls to the entity data or selection set functions.
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

# About Accessing an Object’s Entity Name (AutoLISP)

> An AutoLISP routine must obtain an object’s entity name to make subsequent calls to the entity data or selection set functions.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3EEA3D4B-36B6-4E3B-B9E8-62B6265FA68C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3EEA3D4B-36B6-4E3B-B9E8-62B6265FA68C.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The `entsel`  and `nentsel`  functions prompt the user to interactively select an object in the drawing area and return not only the selected object’s entity name but additional information for the routine's use. The `entsel`  function returns both the entity name of the object selected and the center of the pick box when the pointer button on the input device was clicked.

Some entity operations require knowledge of the point by which the object was selected. Examples from the set of existing AutoCAD commands include: BREAK, TRIM, and EXTEND. The `nentsel`  function returns the same information as the `entsel`  function, except when a complex is selected such as a polyline or block. Both these functions accept keywords if they are preceded by a call to `initget`.

The `entnext`  function retrieves entity names sequentially. If `entnext`  is called with no arguments, it returns the name of the first entity in the drawing database. If its argument is the name of an entity in the current drawing, `entnext`  returns the name of the succeeding entity.

The `entlast`  function retrieves the name of the last entity in the database. The last entity is the most recently created main entity, so `entlast`  can be called to obtain the name of an entity that has just been created with a call to `command`.

You can set the entity name returned by `entnext`  to the same variable name passed to this function. This “walks” a single entity name variable through the database, as shown in the following example code:

```lisp
(setq one_ent (entnext))         ; Gets name of first entity.
(while one_ent
..
                                 ; Processes new entity.
.
(setq one_ent (entnext one_ent))
)                                ; Value of one_ent is now nil.
```

The following example code illustrates how `ssadd`  can be used in conjunction with `entnext`  to create selection sets and add members to an existing set.

```lisp
(setq e1 (entnext))
(if (not e1)                           ; Sets e1 to name of first entity.
  (princ "\nNo entities in drawing. ")
  (progn
    (setq ss (ssadd))                  ; Sets ss to a null selection set.
    (ssadd e1 ss)                      ; Returns selection set ss with e1 added.
    (setq e2 (entnext e1))             ; Gets entity following e1.
    (ssadd e2 ss)                      ; Adds e2 to selection set ss.
  )
)
```
