---
title: "About VLA-objects (AutoLISP/ActiveX)"
guid: "GUID-F6477DD7-7982-4742-95CA-F30BBBBE80B1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F6477DD7-7982-4742-95CA-F30BBBBE80B1.htm"
generated: "2025-11-28T19:06:01.586321Z"
description: Objects in a drawing can be represented as ActiveX (VLA) objects.
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

# About VLA-objects (AutoLISP/ActiveX)

> Objects in a drawing can be represented as ActiveX (VLA) objects.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F6477DD7-7982-4742-95CA-F30BBBBE80B1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F6477DD7-7982-4742-95CA-F30BBBBE80B1.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

Note:
 ActiveX is not supported on Mac OS and Web.

When working with ActiveX methods and properties, you must refer to VLA-objects, not the ename pointer returned by functions such as `entlast`. VLA-objects can be converted to an ename pointer with `vlax-vla-object->ename`. You can also use `vlax-ename->vla-object`  to convert an ename pointer to a VLA-object.
