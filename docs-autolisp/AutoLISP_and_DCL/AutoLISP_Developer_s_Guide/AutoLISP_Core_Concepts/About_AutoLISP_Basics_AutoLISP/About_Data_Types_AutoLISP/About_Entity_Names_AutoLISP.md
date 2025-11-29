---
title: About Entity Names (AutoLISP)
guid: "GUID-9CB07C25-4439-445D-B1B3-92174C53C571"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9CB07C25-4439-445D-B1B3-92174C53C571.htm"
generated: "2025-11-28T19:06:01.506039Z"
description: An entity name is a numeric label assigned to objects in a drawing.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# About Entity Names (AutoLISP)

> An entity name is a numeric label assigned to objects in a drawing.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9CB07C25-4439-445D-B1B3-92174C53C571.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9CB07C25-4439-445D-B1B3-92174C53C571.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

It is actually a pointer into a file maintained by AutoCAD, and can be used to find the object's database record and its vectors (if they are displayed). This label can be referenced by AutoLISP functions to allow selection of objects for processing in various ways. Internally, AutoCAD refers to objects as entities.

Note:
 You can use the
vlax-ename->vla-object
 function to convert an entity name to a VLA-object when working with ActiveX functions. The
vlax-vla-object->ename
 function converts a VLA-object to an entity name. ActiveX is not supported on Mac OS and Web.

The following functions are useful when working with entity names:

- entget
   - Retrieves an object's (entity's) definition data.
- entlast
   - Returns the name of the last non-deleted main object (entity) in the drawing.
- ssname
   - Returns the object (entity) name of the indexed element of a selection set.
- entsel
   - Prompts the user to select a single object (entity) by specifying a point.
- nentsel
   - Prompts the user to select an object (entity) by specifying a point, and provides access to the definition data contained within a complex object.
- nentselp
   - Provides similar functionality to that of the
  nentsel
   function without the need for user input.
- handent
   - Returns an object (entity) name based on its handle.

The following example uses the `entlast`  function to get the name of the last object created in the drawing.

```lisp
(entlast)

<Entity name: 27f0540>
```

Entity names assigned to objects in a drawing are only in effect during the current editing session. The next time you open the drawing, AutoCAD assigns new entity names to the objects. You can use an object's handle to refer to it from one editing session to another.
