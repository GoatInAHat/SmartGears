---
title: About Filtering for Extended Data in a Selection Set (AutoLISP)
guid: "GUID-84C3CDF5-DF88-4473-A10D-C7778EE037C8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-84C3CDF5-DF88-4473-A10D-C7778EE037C8.htm"
generated: "2025-11-28T19:06:11.137735Z"
description: "You can select all entities containing extended data for a particular application using the filter-list argument of ssget."
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

# About Filtering for Extended Data in a Selection Set (AutoLISP)

> You can select all entities containing extended data for a particular application using the filter-list argument of ssget .

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-84C3CDF5-DF88-4473-A10D-C7778EE037C8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-84C3CDF5-DF88-4473-A10D-C7778EE037C8.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The *filter-list*  argument must be a list that contains -3 as its first element. The following example code selects all the objects in a drawing that include extended data for the "APPNAME" application:

```lisp
(ssget "X" '((-3 ("APPNAME"))))
```

You can also expand the scope of the filter to filter specific types of objects. The following example code selects all the circles in a drawing that include extended data for the "APPNAME" application:

```lisp
(ssget "X" '((0 . "CIRCLE") (-3 ("APPNAME"))))
```

If more than one application name is included in the -3 group's list, an `AND`  operation is implied and the entity must contain extended data for all of the specified applications. So, the following statement would select all the objects with extended data for both the "APP1" and "APP2" applications:

```lisp
(ssget "X" '((-3 ("APP1")("APP2"))))
```

Wild-card matching is also permitted, so either of the following statements will select all the objects with extended data for either or both of these applications.

```lisp
(ssget "X" '((-3 ("APP[12]"))))
(ssget "X" '((-3 ("APP1,APP2"))))
```
