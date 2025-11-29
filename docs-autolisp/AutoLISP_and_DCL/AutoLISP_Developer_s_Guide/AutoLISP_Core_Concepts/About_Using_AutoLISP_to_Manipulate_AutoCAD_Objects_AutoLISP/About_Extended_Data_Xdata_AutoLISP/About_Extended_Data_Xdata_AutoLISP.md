---
title: "About Extended Data - Xdata (AutoLISP)"
guid: "GUID-A94BC605-5517-437F-A6FE-D3EB8116A01A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A94BC605-5517-437F-A6FE-D3EB8116A01A.htm"
generated: "2025-11-28T19:06:14.332459Z"
description: Several AutoLISP functions are provided to handle extended data (xdata), which is created by routines written with AutoLISP, ObjectARX, or Managed .NET.
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

# About Extended Data - Xdata (AutoLISP)

> Several AutoLISP functions are provided to handle extended data (xdata), which is created by routines written with AutoLISP, ObjectARX, or Managed .NET.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A94BC605-5517-437F-A6FE-D3EB8116A01A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A94BC605-5517-437F-A6FE-D3EB8116A01A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

If an entity contains xdata, it follows the entity's regular definition data. You can retrieve an entity's extended data by calling `entget`. The `entget`  function retrieves an entity's regular definition data and the xdata for those applications specified in the `entget`  call.

When xdata is retrieved with `entget`, the beginning of extended data is indicated by a -3 dxf group code. The -3 dxf group code is in a list that precedes the first 1001 dxf group code. The 1001 dxf group code contains the application name of the first application retrieved, as shown in the table and as described in the topics in this section.

| Group codes for regular and extended data |  |  |
| --- | --- | --- |
| Group code | Field | Type of data |
| (-1, -2  (0-239     ) | Entity name)  Regular definition data fields)  .  .  . | Normal entity definition data |
| (-3  (1001  (1000,  1002-1071     (1001  (1000,  1002-1071     (1001 | Extended data sentinel  Registered application name 1)   XDATA fields)  .  .  .  Registered application name 2)   XDATA fields)  .  .  .  Registered application name 3)  .  . | Extended data |
