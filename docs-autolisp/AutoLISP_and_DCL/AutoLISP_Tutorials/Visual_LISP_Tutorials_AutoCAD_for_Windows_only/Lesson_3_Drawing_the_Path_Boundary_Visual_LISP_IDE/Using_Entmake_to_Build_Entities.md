---
title: Using Entmake to Build Entities
guid: "GUID-43720E27-7286-401B-A04A-AAEB49CED837"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-43720E27-7286-401B-A04A-AAEB49CED837.htm"
generated: "2025-11-28T19:06:57.713581Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Using Entmake to Build Entities

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-43720E27-7286-401B-A04A-AAEB49CED837.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-43720E27-7286-401B-A04A-AAEB49CED837.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The `entmake`  function allows you to build an entity by gathering values for things such as coordinate location and orientation, layer, and color into an association list, then asking AutoCAD to build the entity for you. The association list you build for the `entmake`  function looks very much like the association list you get back when you call the `entget`  function. The difference is that `entget`  returns information about an entity, while `entmake`  builds a new entity from raw data.
