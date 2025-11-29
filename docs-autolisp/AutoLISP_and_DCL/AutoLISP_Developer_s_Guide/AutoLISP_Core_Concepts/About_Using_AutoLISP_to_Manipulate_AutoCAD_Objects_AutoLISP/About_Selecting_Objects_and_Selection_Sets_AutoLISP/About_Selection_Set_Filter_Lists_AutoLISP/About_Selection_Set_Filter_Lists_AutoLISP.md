---
title: About Selection Set Filter Lists (AutoLISP)
guid: "GUID-7BE77062-C359-4D01-915B-69CF672C653B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7BE77062-C359-4D01-915B-69CF672C653B.htm"
generated: "2025-11-28T19:06:10.851547Z"
description: An entity filter list is an association list that uses DXF group codes in the same format as a list returned by entget.
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

# About Selection Set Filter Lists (AutoLISP)

> An entity filter list is an association list that uses DXF group codes in the same format as a list returned by entget .

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7BE77062-C359-4D01-915B-69CF672C653B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7BE77062-C359-4D01-915B-69CF672C653B.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The `ssget`  function recognizes all group codes except entity names (group code -1), handles (group code 5), and xdata (group codes greater than 1000). If an invalid group code is used in a filter-list, it is ignored by `ssget`. Use the group code -3 to search for objects with xdata. When a filter-list is provided as the last argument to `ssget`, the function scans the selected objects and creates a selection set containing the names of all main entities matching the specified criteria. The *filter-list*  specifies which property (or properties) of the entities are to be checked and which values constitute a match.

For example, you can obtain a selection set that includes all objects of a given type, on a given layer, or of a given color.

The following examples demonstrate different methods of using a *filter-list*  with various object selection options.

| SSGET examples using filter lists |  |
| --- | --- |
| Function call | Effect |
| `(setq ss1`  `(ssget '((0 . "TEXT")))`  `)` | Prompts for general object selection but adds only text objects to the selection set. |
| `(setq ss1`  `(ssget "P" '((0 . "LINE")))`  `)` | Creates a selection set containing all line objects from the last selection set created. |
| `(setq ss1`  `(ssget "W" pt1 pt2 '((8 . "FLOOR9")))`  `)` | Creates a selection set of all objects inside the window that are also on layer FLOOR9. |
| `(setq ss1`  `(ssget "X" '((0 . "CIRCLE")))`  `)` | Creates a selection set of all objects in the database that are Circle objects. |
| `(setq ss1`  `(ssget "I" '((0 . "LINE") (62 . 5)))`  `)` | Creates a selection set of all blue Line objects that are part of the Implied selection set (those objects selected while the AutoCAD PICKFIRST system variable is in effect).  Note that this filter picks up lines that have been assigned color 5 (blue), but not blue lines that have had their color applied by the ByLayer or ByBlock properties. |

If both the group code and the desired value are known, the list may be quoted as shown previously. If either is specified by a variable, the list must be constructed using the `list`  and `cons`  function. For example, the following code creates a selection set of all objects in the database that are on layer FLOOR3:

```lisp
(setq lay_name "FLOOR3")
(setq ss1
  (ssget "X"
    (list (cons 8 lay_name))
  )
)
```

If the *filter-list*  specifies more than one property, an entity is included in the selection set only if it matches all specified conditions, as in the following example code:

```lisp
(ssget "X" (list (cons 0 "CIRCLE")(cons 8 lay_name)(cons 62 3)))
```

This code selects only Circle objects on layer FLOOR3 that are colored green. This type of test performs a Boolean `“AND”`  operation.

The `ssget`  function filters a selection set by scanning the selected entities and comparing the fields of each main entity against the specified filtering list. If an entity's properties match all specified fields in the filtering list, it is included in the returned selection set. Otherwise, the entity is not included in the selection set. The `ssget`  function returns `nil`  if none of the selected entities match the specified filtering criteria.

Note:
 The meaning of certain group codes can differ from entity to entity, and not all group codes are present in all entities. If a particular group code is specified in a filter, entities not containing that group code are excluded from the selection set that
ssget
 returns.

When `ssget`  filters a selection set, the selected objects it retrieves might include entities from both paper space and model space. However, when the selection set is passed to an AutoCAD command, only entities from the space that is currently in effect are used. (The space to which an entity belongs is specified by the value of its 67 group code.)
