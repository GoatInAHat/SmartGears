---
title: About Using AutoLISP to Manipulate AutoCAD Objects (AutoLISP)
guid: "GUID-9AED35A1-5CB2-43D8-88ED-CBE95E54798D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9AED35A1-5CB2-43D8-88ED-CBE95E54798D.htm"
generated: "2025-11-28T19:06:10.697081Z"
description: You can select and handle objects, and use their extended data.
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

# About Using AutoLISP to Manipulate AutoCAD Objects (AutoLISP)

> You can select and handle objects, and use their extended data.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9AED35A1-5CB2-43D8-88ED-CBE95E54798D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9AED35A1-5CB2-43D8-88ED-CBE95E54798D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Most AutoLISP ^®^  functions that handle selection sets and objects identify a set or an object by the entity name. For selection sets, which are valid only in the current session, the volatility of names poses no problem, but it does for objects because they are saved in the drawing database. An application that must refer to the same objects in the same drawing (or drawings) at different times can use the objects' handles.

AutoLISP uses symbol tables to maintain lists of graphic and non-graphic data related to a drawing, such as the layers, linetypes, and block definitions. Each symbol table entry has a related entity name and handle and can be manipulated in a manner similar to the way other AutoCAD ^®^  entities are manipulated.
