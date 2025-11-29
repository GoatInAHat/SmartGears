---
title: About Deleting an Entity (AutoLISP)
guid: "GUID-7DC692D0-4FD4-4931-93C3-EFC616397E4C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7DC692D0-4FD4-4931-93C3-EFC616397E4C.htm"
generated: "2025-11-28T19:06:13.309779Z"
description: Entities can be deleted using the entdel function or AutoCAD ERASE command (with command).
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

# About Deleting an Entity (AutoLISP)

> Entities can be deleted using the entdel function or AutoCAD ERASE command (with command ).

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7DC692D0-4FD4-4931-93C3-EFC616397E4C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7DC692D0-4FD4-4931-93C3-EFC616397E4C.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 12/08/2024

Entities are not purged from the database until the end of the current drawing session, so if the application calls `entdel`  on an entity that was deleted during that session, the entity is undeleted.

Attributes and "legacy" polyline vertices cannot be deleted independently of their parent entities. The `entdel`  function and AutoCAD ERASE command only operate on main entities. If you need to delete an attribute or vertex, you can use the AutoCAD ATTEDIT or PEDIT commands with `command`.
