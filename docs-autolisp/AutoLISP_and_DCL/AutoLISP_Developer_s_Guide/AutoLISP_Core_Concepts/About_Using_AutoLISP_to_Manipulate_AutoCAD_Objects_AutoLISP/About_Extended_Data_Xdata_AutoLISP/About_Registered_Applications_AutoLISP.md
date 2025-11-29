---
title: About Registered Applications (AutoLISP)
guid: "GUID-8F373105-67D5-4CBD-931D-D0869F2A7EB5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-8F373105-67D5-4CBD-931D-D0869F2A7EB5.htm"
generated: "2025-11-28T19:06:14.534887Z"
description: An application must register its name or names to be recognized by AutoCAD.
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

# About Registered Applications (AutoLISP)

> An application must register its name or names to be recognized by AutoCAD.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-8F373105-67D5-4CBD-931D-D0869F2A7EB5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-8F373105-67D5-4CBD-931D-D0869F2A7EB5.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Extended data must contain an application name before it can be attached to an entity and that application name must also exist in the APPID symbol table. Registration is done with the `regapp`  function, which specifies a string to use as an application name. If it successfully adds the name to APPID, it returns the name of the application; otherwise it returns `nil`. A result of `nil`  indicates that the name is already present in the symbol table. This is not an actual error condition but an expected return value, because the application name needs to be registered only once per drawing.

Before you register an application, you should first check to see if the name is not already in the APPID symbol table. If the name is not there, the application must register it. Otherwise, it can simply go ahead and attach the extended data to an entity for the application.

The following example code demonstrates the typical use of `regapp`.

```lisp
(setq appname "MYAPP_2356")                            ; Unique application name.
(if (tblsearch "appid" appname)                        ; Checks if already registered.
  (princ (strcat "\n" appname " already registered."))

  (if (= (regapp appname) nil)                         ; Some other problem.
    (princ (strcat "\nCan't register XDATA for " appname ". "))
  )
)
```

The `regapp`  function provides a measure of security, but it cannot guarantee that two separate applications have not chosen the same name. One way of ensuring this is to adopt a naming scheme that uses the company or product name and a unique number (like your telephone number or the current date and time).
