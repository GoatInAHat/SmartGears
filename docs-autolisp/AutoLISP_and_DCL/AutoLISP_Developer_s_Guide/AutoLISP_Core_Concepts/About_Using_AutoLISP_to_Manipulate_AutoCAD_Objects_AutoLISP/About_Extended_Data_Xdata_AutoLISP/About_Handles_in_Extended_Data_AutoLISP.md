---
title: About Handles in Extended Data (AutoLISP)
guid: "GUID-61A8B084-BA16-4FDA-8C8A-C2FA73E8DDC9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-61A8B084-BA16-4FDA-8C8A-C2FA73E8DDC9.htm"
generated: "2025-11-28T19:06:14.995788Z"
description: Extended data can contain handles (dxf group code 1005) to save relational structures within a drawing.
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

# About Handles in Extended Data (AutoLISP)

> Extended data can contain handles (dxf group code 1005) to save relational structures within a drawing.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-61A8B084-BA16-4FDA-8C8A-C2FA73E8DDC9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-61A8B084-BA16-4FDA-8C8A-C2FA73E8DDC9.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

This allows you to build relationships between two or more entities by saving the handle of one object to another objects’s xdata. The handle can be retrieved later from the xdata and passed to `handent`  to obtain the other entity. Because more than one entity can reference another, handles in xdata might not necessarily be unique. The AutoCAD AUDIT command does require that handles in extended data either be `NULL`  or valid entity handles (within the current drawing). The best way to ensure that xdata entity handles are valid is to obtain a referenced entity's handle directly from its definition data by means of `entget`. The handle value is in dxf group code 5.

When you reference entities in other drawings (for example, entities that are attached with AutoCAD XREF command), you can avoid protests from the AutoCAD AUDIT command by using extended entity strings (dxf group code 1000) rather than handles (dxf group code 1005). The handles of cross-referenced entities are either not valid in the current drawing, or they conflict with valid handles. However, if an xref attachment changes to a bound xref or is combined with the current drawing in some other way, it is up to the application to revise the entity references accordingly.

When drawings are combined by means of INSERT, INSERT*, or XREF Bind (XBIND), or partial DXFIN, handles are translated so they become valid in the current drawing. (If the incoming drawing did not employ handles, new ones are assigned.) Extended entity handles that refer to incoming entities are also translated when these commands are invoked.

When an entity is placed in a block definition (with the AutoCAD BLOCK command), the entity within the block is assigned new handles. (If the original entity is restored by means of the AutoCAD OOPS command, it retains its original handles.) The value of any xdata handles remains unchanged. When a block is exploded (with the AutoCAD EXPLODE command), xdata handles are translated in a manner similar to the way they are translated when drawings are combined. If the xdata handle refers to an entity that is not within the block, it is unchanged. However, if the xdata handle refers to an entity that is within the block, the data handle is assigned the value of the new (exploded) entity's handle.
