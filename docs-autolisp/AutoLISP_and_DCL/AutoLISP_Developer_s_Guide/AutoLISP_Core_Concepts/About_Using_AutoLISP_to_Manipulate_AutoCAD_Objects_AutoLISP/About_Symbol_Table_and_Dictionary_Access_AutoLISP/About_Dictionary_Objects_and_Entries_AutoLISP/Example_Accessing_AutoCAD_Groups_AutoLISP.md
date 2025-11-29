---
title: "Example: Accessing AutoCAD Groups (AutoLISP)"
guid: "GUID-74E761F9-08B1-4690-ADA0-396BC9431729"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-74E761F9-08B1-4690-ADA0-396BC9431729.htm"
generated: "2025-11-28T19:06:15.651369Z"
description: This example demonstrates one method for accessing the entities contained in a group.
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

# Example: Accessing AutoCAD Groups (AutoLISP)

> This example demonstrates one method for accessing the entities contained in a group.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-74E761F9-08B1-4690-ADA0-396BC9431729.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-74E761F9-08B1-4690-ADA0-396BC9431729.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

This example assumes a group named G1 exists in the current drawing.

```lisp
(setq objdict (namedobjdict))
(setq grpdict (dictsearch objdict "ACAD_GROUP"))
```

This sets the `grpdict`  variable to the entity definition list of the ACAD_GROUP dictionary and returns the following:

```lisp
((-1 . <Entity name: 8dc10468>) (0 . "DICTIONARY") (5 . "D")
(102 . "{ACAD_REACTORS") (330 . <Entity name: 8dc10460>)
(102 . "}") (100 . "AcDbDictionary") (3 . "G1")
(350 . <Entity name: 8dc41240>))
```

The following code sets the variable `group1`  to the entity definition list of the G1 group:

```lisp
(setq group1 (dictsearch (cdar grpdict) "G1"))

((-1 . <Entity name: 8dc10518>) (0 . "GROUP") (5 . "23")
(102 . "{ACAD_REACTORS") (330 . <Entity name: 8dc10468>)
(102 . "}") (100 . "AcDbGroup") (300 . "line and circle") (70 . 0) (71
. 1) (340 . <Entity name: 8dc10510>)(340 . <Entity name: 8dc10550>))
```

The 340 group codes are the entities that belong to the group.
