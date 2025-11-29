---
title: About Retrieving Extended Data (AutoLISP)
guid: "GUID-1A2A0518-E9DB-462E-925E-32181D96CE4D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1A2A0518-E9DB-462E-925E-32181D96CE4D.htm"
generated: "2025-11-28T19:06:14.697427Z"
description: An application can obtain the extended data (xdata) that it has attached to an entity with entget.
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

# About Retrieving Extended Data (AutoLISP)

> An application can obtain the extended data (xdata) that it has attached to an entity with entget .

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1A2A0518-E9DB-462E-925E-32181D96CE4D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1A2A0518-E9DB-462E-925E-32181D96CE4D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The `entget`  function can return an entity’s definition data and the xdata for the applications it requests. It requires an additional argument, application, that specifies the application names. The names passed to `entget`  must correspond to applications registered by a previous call to `regapp`; they can also contain wild-card characters.

By default, associative hatch patterns contain xdata. The following example code demonstrates the association list of this xdata. Before you can use the code, create a closed boundary and apply an associative hatch object to the boundary.

```lisp
(entget (car (entsel)) '("ACAD"))
```

Select object: *Select an associative hatch*

Entering the preceding code at the command line returns a list that looks something like this:

```lisp
((-1 . <Entity name: 7ffffb05e10>) (0 . "HATCH") (330 . <Entity name: 7ffffb039f0>) (5 . "1D9") (100 . "AcDbEntity") (67 . 0) (410 . "Model") (8 . "0") (100 . "AcDbHatch") (10 0.0 0.0 0.0) (210 0.0 0.0 1.0) (2 . "ANSI31") (70 . 0) (71 . 1) (91 . 1) (92 . 7) (72 . 0) (73 . 1) (93 . 4) (10 31.2567 17.3197 0.0) (10 7.77575 17.3197 0.0) (10 7.77575 8.83313 0.0) (10 31.2567 8.83313 0.0) (97 . 1) (330 . <Entity name: 7ffffb05d50>) (75 . 1) (76 . 1) (52 . 0.0) (41 . 3.0) (77 . 0) (78 . 1) (53 . 0.785398) (43 . 0.0) (44 . 0.0) (45 . -0.265165) (46 . 0.265165) (79 . 0) (47 . 0.0289642) (98 . 1) (10 21.1106 14.5391 0.0) (-3 ("ACAD" (1010 0.0 0.0 0.0))))
```

The following example code demonstrates a typical sequence for retrieving xdata for two specified applications. Note that the *application*  argument accepts application names in list form:

```lisp
(setq working_elist
  (entget ent_name
    '("MY_APP_1" "SOME_OTHER") ; Only xdata from "MY_APP_1"
  )                            ; and "SOME_OTHER" is retrieved.
)
(if working_elist
  (progn
    ...                        ; Updates working entity groups.
    (entmod working_elist)     ; Only xdata from registered
  )                            ; applications still in the
)                              ; working_elist list are modified.
```

As the example code demonstrates, you can modify xdata retrieved by `entget`  by using a subsequent call to `entmod`, just as you can use `entmod`  to modify normal entity definition data. You can also create xdata by defining it in the entity list passed to `entmake`. Returning the xdata of only those applications specifically requested protects one application from corrupting another application's data. It also controls the amount of memory that an application needs to use and simplifies the xdata processing that an application needs to perform.

Note:
 Because the strings passed by
application
 can include wild-card characters, an application name of
"*"
 will cause
entget
 to return all extended data attached to an entity.
