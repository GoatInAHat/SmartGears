---
title: namedobjdict (AutoLISP)
guid: "GUID-A1E43B30-EF8C-452E-97F5-8AC201E310EE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A1E43B30-EF8C-452E-97F5-8AC201E310EE.htm"
generated: "2025-11-28T19:06:37.402163Z"
description: Returns the entity name of the current drawing's named object dictionary, which is the root of all nongraphical objects in the drawing
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

# namedobjdict (AutoLISP)

> Returns the entity name of the current drawing's named object dictionary, which is the root of all nongraphical objects in the drawing

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A1E43B30-EF8C-452E-97F5-8AC201E310EE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A1E43B30-EF8C-452E-97F5-8AC201E310EE.htm)
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
(namedobjdict)
```

No arguments.

## Return Values

**Type:**  Ename (entity name)

Always returns an ename.

## Remarks

Using the name returned by this function and the dictionary access functions, an application can access the nongraphical objects in the drawing.

## Examples

```lisp
(namedobjdict)

<Entity name: 7ffffb038c0>
```
