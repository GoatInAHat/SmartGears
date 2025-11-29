---
title: About Attaching Extended Data to an Entity (AutoLISP)
guid: "GUID-FA8BBFB8-5C3E-4742-A3A2-CBCDB168FB08"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-FA8BBFB8-5C3E-4742-A3A2-CBCDB168FB08.htm"
generated: "2025-11-28T19:06:14.822941Z"
description: You can use extended data (xdata) to store any type of information you want on an entity.
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

# About Attaching Extended Data to an Entity (AutoLISP)

> You can use extended data (xdata) to store any type of information you want on an entity.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-FA8BBFB8-5C3E-4742-A3A2-CBCDB168FB08.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-FA8BBFB8-5C3E-4742-A3A2-CBCDB168FB08.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

The xdata attached to an entity might be a record in an external database, a date and time stamp when an entity was added or modified, or contain information that represents an item in the real-world such as a telephone or workstation. Since xdata is hidden from the user, it makes it harder to modify without using a custom application.

Note:
 Xdata attached to an entity is maintained when an object is copied in the current drawing or between drawings.

The following example code demonstrates the basics of attaching xdata to the last entity added to a drawing. Before executing the following example code, draw an entity (such as a line or a circle):

```lisp
; Gets the association list of definition data
; for the last entity.
(setq lastent (entget (entlast)))

; Registers the application name.
(regapp "NEWDATA")

(setq exdata                        ; Sets the variable
  '((-3 ("NEWDATA"                  ; exdata equal to the
    (1000 . "This is a new thing!") ; new extended data—
  )))                               ; in this case, a text
)                                   ; string.

; Appends new data list to entity's list.
(setq newent
  (append lastent exdata))

; Modifies the entity with the new definition data.
(entmod newent)
```

The following example code can be used to verify that your new xdata has been attached to the entity:

```lisp
(entget (car (entsel)) '("NEWDATA"))
```
