---
title: About Modifying Selection Sets (AutoLISP)
guid: "GUID-BF30AA6B-AB31-4085-BEC8-ABA518E9DD3C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-BF30AA6B-AB31-4085-BEC8-ABA518E9DD3C.htm"
generated: "2025-11-28T19:06:11.391112Z"
description: Once a selection set has been created, you can add entities to it or remove entities from it with ssadd and ssdel.
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

# About Modifying Selection Sets (AutoLISP)

> Once a selection set has been created, you can add entities to it or remove entities from it with ssadd and ssdel .

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-BF30AA6B-AB31-4085-BEC8-ABA518E9DD3C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-BF30AA6B-AB31-4085-BEC8-ABA518E9DD3C.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

You can use the `ssadd`  function to create a new selection set or add entities to an existing selection set. The following example code creates a selection set that includes the first and last entities in the current drawing (`entnext`  and `entlast`):

```lisp
(setq fname (entnext))                 ; Gets first entity in the drawing.
(setq lname (entlast))                 ; Gets last entity in the drawing.
(if (not fname)
  (princ "\nNo entities in drawing. ")
  (progn
    (setq ourset (ssadd fname))        ; Creates a selection set
                                       ; of the first entity.
    (ssadd lname ourset)               ; Adds the last entity to
                                       ; the selection set.
  )
)
```

The example runs correctly even if only one entity is in the database (in which case both `entnext`  and `entlast`  set their arguments to the same entity name). If `ssadd`  is passed the name of an entity already in the selection set, it ignores the request and does not report an error.

The following example code removes the first entity from the selection set created in the previous example:

```lisp
(ssdel fname ourset)
```

If there is more than one entity in the drawing (that is, if `fname`  and `lname`  are not equal), then the selection set `ourset`  contains only `lname`, the last entity in the drawing.

The function `sslength`  returns the number of entities in a selection set, and `ssmemb`  tests whether a particular entity is a member of a selection set. Finally, the function `ssname`  returns the name of a particular entity in a selection set, using an index to the set (entities in a selection set are numbered from 0).

The following example code shows calls to `ssname`:

```lisp
(setq sset (ssget))                     ; Prompts the user to create
                                        ; a selection set.
(setq ent1 (ssname sset 0))             ; Gets the name of the first
                                        ; entity in sset.
(setq ent4 (ssname sset 3))             ; Gets the name of the fourth
                                        ; entity in sset.
(if (not ent4)
  (princ "\nNeed to select at least four entities. ")
)
(setq ilast (sslength sset))            ; Finds index of the last
                                        ; entity in sset.
                                        ; Gets the name of the last
                                        ; entity in sset.
(setq lastent (ssname sset (1- ilast)))
```

Regardless of how entities are added to a selection set, the set never contains duplicate entities. If the same entity is added more than once, the later additions are ignored. Therefore, `sslength`  accurately returns the number of distinct entities in the specified selection set.
