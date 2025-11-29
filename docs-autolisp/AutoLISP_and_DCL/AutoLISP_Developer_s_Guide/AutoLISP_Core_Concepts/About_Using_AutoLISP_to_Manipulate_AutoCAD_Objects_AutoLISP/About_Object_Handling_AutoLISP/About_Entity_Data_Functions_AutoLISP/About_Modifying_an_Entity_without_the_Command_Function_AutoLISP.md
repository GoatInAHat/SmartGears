---
title: About Modifying an Entity without the Command Function (AutoLISP)
guid: "GUID-125DC058-BBAA-4CA9-A203-53F0A27B87D0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-125DC058-BBAA-4CA9-A203-53F0A27B87D0.htm"
generated: "2025-11-28T19:06:13.133361Z"
description: An entity can be modified directly by changing its entity list and posting the changes back to the database.
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

# About Modifying an Entity without the Command Function (AutoLISP)

> An entity can be modified directly by changing its entity list and posting the changes back to the database.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-125DC058-BBAA-4CA9-A203-53F0A27B87D0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-125DC058-BBAA-4CA9-A203-53F0A27B87D0.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The `entmod`  function modifies an entity by passing it a list in the same format as a list returned by `entget`  but with some of the entity group code values (presumably) modified by the application. This function complements `entget`. The primary mechanism by which an AutoLISP application updates the database is by retrieving an entity with `entget`, modifying its entity list, and then passing the list back to the database with `entmod`.

The following example code retrieves the definition data of the first entity in the drawing and changes its layer property to MYLAYER.

```lisp
(setq en (entnext))         ; Sets en to first entity name in the drawing.
(setq ed (entget en))       ; Sets ed to the entity data for entity name en.
(setq ed
  (subst (cons 8 "MYLAYER")
    (assoc 8 ed)            ; Changes the layer group in ed.
    ed                      ; to layer MYLAYER.
  )
)
(entmod ed)                 ; Modifies entity en's layer in the drawing.
```

There are restrictions on the changes to the database that `entmod`  can make; `entmod`  cannot change the following:

- The entity's type or handle.
- Internal fields. (Internal fields are the values that AutoCAD assigns to certain group codes: -2, entity name reference; -1, entity name; 5, entity handle.) Any attempt to change an internal field—for example, the main entity name in a Seqend subentity (group code -2)—is ignored.
- Viewport entities. An attempt to change a viewport entity causes an error.

Other restrictions apply when modifying dimensions and hatch patterns.

AutoCAD must recognize all objects (except layers) that the entity list refers to. The name of any text style, linetype, shape, or block that appears in an entity list must be defined in the current drawing before the entity list is passed to `entmod`. The one exception is that `entmod`  accepts new layer names. If the entity list refers to a layer name that has not been defined in the current drawing, `entmod`  creates a new layer. The attributes of the new layer are the standard default values used by the New option of the AutoCAD LAYER command.

The `entmod`  function can modify subentities such as polyline vertices and block attributes. If you use `entmod`  to modify an entity in a block definition, this affects all references to that block which exist in model space and paper space. Attributes, unless defined as constant, are not updated for each block reference that exists in a drawing. Also, entities in block definitions cannot be deleted by `entdel`.
