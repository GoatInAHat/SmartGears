---
title: About Passing Pick Points to AutoCAD Commands (AutoLISP)
guid: "GUID-EF2D73CE-9B75-4532-AF2F-DD6F0C36C3FD"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EF2D73CE-9B75-4532-AF2F-DD6F0C36C3FD.htm"
generated: "2025-11-28T19:06:06.667939Z"
description: Some AutoCAD commands (such as TRIM, EXTEND, and FILLET) require the user to specify a pick point as well as the object itself.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# About Passing Pick Points to AutoCAD Commands (AutoLISP)

> Some AutoCAD commands (such as TRIM, EXTEND, and FILLET) require the user to specify a pick point as well as the object itself.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EF2D73CE-9B75-4532-AF2F-DD6F0C36C3FD.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EF2D73CE-9B75-4532-AF2F-DD6F0C36C3FD.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

Object and point data can be passed to the `command`  and `command-s`  functions without the use of a `PAUSE`, but requires you to first store the values as variables. Points can be passed as strings within the `command`  and `command-s`  functions, or can be defined outside the function and passed as variables, as shown in the following example.

The following example code demonstrates one method of passing an entity name and a pick point to the `command`  function.

```lisp
(command "._circle" "5,5" "2")
    ;Draws a circle

(command "._line" "3,5" "7,5" "")
 ;Draws a line

(setq el (entlast))
               ;Gets the last entity
                                  ;  added to the drawing

(setq pt '(5 7))
                  ;Sets the trim point

(command "._trim" el "" pt "")
    ;Performs the trim
```

If AutoCAD is at an idle Command prompt when these statements are called, AutoCAD performs the following actions:

1. Draws a circle centered at (5,5) with a radius of 2.
2. Draws a line from (3,5) to (7,5).
3. Creates a variable
   el
    that is the name of the last object added to the database.
4. Creates a
   pt
    variable that is a point on the circle. (This point selects the portion of the circle to be trimmed.)
5.  Performs the TRIM command by selecting the
   el
    object (the line) and by selecting the point specified by
   pt
   .
