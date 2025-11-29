---
title: Selection Set Manipulation Functions Reference (AutoLISP)
guid: "GUID-43715289-5715-4F96-8D48-3169FA26777A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-43715289-5715-4F96-8D48-3169FA26777A.htm"
generated: "2025-11-28T19:06:18.744901Z"
topic_type: concept
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
---

# Selection Set Manipulation Functions Reference (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-43715289-5715-4F96-8D48-3169FA26777A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-43715289-5715-4F96-8D48-3169FA26777A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

The following table provides summary descriptions of the AutoLISP selection set manipulation functions.

| Selection set manipulation functions | Platforms |  |  |  |  |  |
| --- | --- | --- | --- | --- | --- | --- |
| Windows | Mac OS | Web |  |  |  |  |
| Function | Description | AutoCAD | AutoCAD LT | AutoCAD | AutoCAD LT | AutoCAD |
| [(ssadd *[ename [ss]]*)](../S_Functions/ssadd_AutoLISP.md) | Adds an object (entity) to a selection set, or creates a new selection set | ✓ | ✓ | ✓ | -- | ✓ |
| [(ssdel *ename ss*)](../S_Functions/ssdel_AutoLISP.md) | Deletes an object (entity) from a selection set | ✓ | ✓ | ✓ | -- | ✓ |
| [(ssget *[mode] [pt1 [pt2]] [pt-list] [filter-list]*)](../S_Functions/ssget_AutoLISP.md) | Prompts the user to select objects (entities), and returns a selection set | ✓ | ✓ | ✓ | -- | ✓ |
| [(ssgetfirst)](../S_Functions/ssgetfirst_AutoLISP.md) | Determines which objects are selected and gripped | ✓ | ✓ | ✓ | -- | ✓ |
| [(sslength *ss*)](../S_Functions/sslength_AutoLISP.md) | Returns an integer containing the number of objects (entities) in a selection set | ✓ | ✓ | ✓ | -- | ✓ |
| [(ssmemb *ename ss*)](../S_Functions/ssmemb_AutoLISP.md) | Tests whether an object (entity) is a member of a selection set | ✓ | ✓ | ✓ | -- | ✓ |
| [(ssname *ss index*)](../S_Functions/ssname_AutoLISP.md) | Returns the object (entity) name of the indexed element of a selection set | ✓ | ✓ | ✓ | -- | ✓ |
| [(ssnamex *ss index*)](../S_Functions/ssnamex_AutoLISP.md) | Retrieves information about how a selection set was created | ✓ | ✓ | ✓ | -- | ✓ |
| [(sssetfirst *gripset [pickset]*)](../S_Functions/sssetfirst_AutoLISP.md) | Sets which objects are selected and gripped | ✓ | ✓ | ✓ | -- | ✓ |
