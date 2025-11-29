---
title: About Adding an Entity without Using the Command Function (AutoLISP)
guid: "GUID-DFDAE6CD-E753-4D01-9D9B-4D1F66B1DE6E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-DFDAE6CD-E753-4D01-9D9B-4D1F66B1DE6E.htm"
generated: "2025-11-28T19:06:12.591491Z"
description: An application can add an entity to the drawing database by calling the entmake function.
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

# About Adding an Entity without Using the Command Function (AutoLISP)

> An application can add an entity to the drawing database by calling the entmake function.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-DFDAE6CD-E753-4D01-9D9B-4D1F66B1DE6E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-DFDAE6CD-E753-4D01-9D9B-4D1F66B1DE6E.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 12/08/2024

Like that of `entmod`, the argument to `entmake`  is a list whose format is similar to that returned by `entget`. The new entity that the list describes is appended to the drawing database (it becomes the last entity in the drawing). If the entity is a complex entity (a block, polyface mesh, or "legacy" polyline), it is not appended to the database until it is complete.

The following example code creates a circle on the MYLAYER layer:

```lisp
(entmake '((0 . "CIRCLE") ; Object type
  (8 . "MYLAYER")         ; Layer
  (10 5.0 7.0 0.0)        ; Center point
  (40 . 1.0)              ; Radius
))
```

The following `entmake`  restrictions apply to all entities:

- The first or second member in the list must specify the entity type. The type must be a valid DXF group code. If the first member does not specify the type, it can specify only the name of the entity: group -1 (the name is not saved in the database).
-  AutoCAD must recognize all objects that the entity list refers to. There is one exception:
  entmake
   accepts new layer names.
- Any internal fields passed to
  entmake
   are ignored.
- entmake
   cannot create viewport entities.

For entity types introduced in AutoCAD Release 13 and later releases, you must also specify subclass markers (DXF group code 100) when creating the entity. All AutoCAD entities have the AcDbEntity subclass marker, and this must be explicitly included in the `entmake`  list. In addition, one or more subclass marker entries are required to identify the specific sub-entity type. These entries must follow group code 0 and must precede group codes that are specifically used to define entity properties in the `entmake`  list. For example, the following is the minimum code required to create a MTEXT entity with `entmake`:

```lisp
(entmake '(
  (0 . "MTEXT")
  (100 . "AcDbEntity") ; Required for all post-R12 entities.
  (8 . "ALAYER")
  (100 . "AcDbMText")  ; Identifies the entity as MTEXT.
  (10 4.0 4.0 0.0)
  (1 . "Some\\Ptext")
))
```

The following table identifies the entities that do not require subentity marker entries in the list passed to `entmake`:

| DXF names of entities introduced prior to AutoCAD Release 13 |  |
| --- | --- |
| 3DFACE | ARC |
| ATTDEF | ATTRIB |
| CIRCLE | DIMENSION |
| INSERT | LINE |
| POINT | POLYLINE ("legacy" 2D and 3D) |
| SEQEND | SHAPE |
| SOLID | TEXT |
| VERTEX | VIEWPORT |

The `entmake`  function verifies that a valid layer name, linetype name, and color are supplied. If a new layer name is introduced, `entmake`  automatically creates the new layer. Objects created on a frozen layer are not regenerated until the layer is thawed. The `entmake`  function also checks for block names, dimension style names, text style names, and shape names, if the entity type requires them. The function fails if it cannot create valid entities.
