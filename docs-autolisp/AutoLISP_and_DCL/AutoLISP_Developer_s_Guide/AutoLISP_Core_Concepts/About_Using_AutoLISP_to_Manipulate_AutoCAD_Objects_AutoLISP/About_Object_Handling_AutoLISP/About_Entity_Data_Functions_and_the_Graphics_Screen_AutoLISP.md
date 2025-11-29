---
title: About Entity Data Functions and the Graphics Screen (AutoLISP)
guid: "GUID-34D3F07F-B93F-4316-B683-6D42E3436E27"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-34D3F07F-B93F-4316-B683-6D42E3436E27.htm"
generated: "2025-11-28T19:06:13.560296Z"
description: Changes to the drawing made by the entity data functions are reflected on the graphics screen, provided the entity being deleted, undeleted, modified, or created is in an area and on a layer that is currently visible.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 12/08/2024
topic_subtype:
  - autolisp
---

# About Entity Data Functions and the Graphics Screen (AutoLISP)

> Changes to the drawing made by the entity data functions are reflected on the graphics screen, provided the entity being deleted, undeleted, modified, or created is in an area and on a layer that is currently visible.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-34D3F07F-B93F-4316-B683-6D42E3436E27.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-34D3F07F-B93F-4316-B683-6D42E3436E27.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 12/08/2024

There is one exception; when `entmod`  modifies a subentity, it does not update the image of the entire (complex) entity. If, for example, an application modifies 100 vertices of a "legacy" 2D polyline with 100 calls to `entmod`, the time required to recalculate and redisplay the entire polyline is unacceptably slow. Instead, an application can perform a series of subentity modifications, and then redisplay the entire entity with a single call to the `entupd`  function.

Consider the following; if the first entity in the current drawing is lightweight polyline with several vertices, the following code modifies the second vertex of the polyline and regenerates its display.

```lisp
(setq e1 (entnext))    ; Sets e1 to the polyline's entity name.
(setq v1 (entnext e1)) ; Sets v1 to its first vertex.
(setq v2 (entnext v1)) ; Sets v2 to its second vertex.
(setq v2d (entget v2)) ; Sets v2d to the vertex data.
(setq v2d
  (subst
    '(10 1.0 2.0 0.0)
    (assoc 10 v2d)     ; Changes the vertex's location in v2d
    v2d                ; to point (1,2,0).
  )
)
(entmod v2d)           ; Moves the vertex in the drawing.
(entupd e1)            ; Regenerates the polyline entity e1.
```

The argument to `entupd`  can specify either a main entity or a subentity. In either case, `entupd`  regenerates the entire entity. Although its primary use is for complex entities, `entupd`  can regenerate any entity in the current drawing.

Note:
 To ensure that all instances of the block references are updated, you must regenerate the drawing by invoking the AutoCAD REGEN command (with
command
). The
entupd
 function is not sufficient if the modified entity is in a block definition.
