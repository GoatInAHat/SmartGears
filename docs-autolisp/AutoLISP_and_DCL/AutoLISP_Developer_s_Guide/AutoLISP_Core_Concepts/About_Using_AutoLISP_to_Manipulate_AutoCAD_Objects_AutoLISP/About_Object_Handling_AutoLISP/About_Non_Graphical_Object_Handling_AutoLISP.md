---
title: "About Non-Graphical Object Handling (AutoLISP)"
guid: "GUID-984A6964-E801-4C22-8E41-BF3D05CD122F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-984A6964-E801-4C22-8E41-BF3D05CD122F.htm"
generated: "2025-11-28T19:06:13.635694Z"
description: "A drawing database contains two types of non-graphical objects: dictionary and symbol table objects."
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

# About Non-Graphical Object Handling (AutoLISP)

> A drawing database contains two types of non-graphical objects: dictionary and symbol table objects.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-984A6964-E801-4C22-8E41-BF3D05CD122F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-984A6964-E801-4C22-8E41-BF3D05CD122F.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Although there are similarities between these object types, they are handled differently. All object types are supported by the `entget`, `entmod`, `entdel`, and `entmake`  functions, although object types individually dictate their participation in these functions and may refuse any or all processing. With respect to AutoCAD built-in objects, the rules apply for symbol tables and for dictionary objects.

All rules and restrictions that apply to graphical objects apply to non-graphical objects as well. Non-graphical objects cannot be passed to the `entupd`  function. When using `entmake`, the object type determines where the object will reside. For example, if a layer object is passed to `entmake`, it automatically goes to the layer symbol table. If a graphical object is passed to `entmake`, it will reside in the current space (model or paper).
