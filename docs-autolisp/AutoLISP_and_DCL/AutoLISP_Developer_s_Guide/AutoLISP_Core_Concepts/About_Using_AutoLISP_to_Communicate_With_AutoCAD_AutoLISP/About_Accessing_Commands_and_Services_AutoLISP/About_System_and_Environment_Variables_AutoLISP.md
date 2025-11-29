---
title: About System and Environment Variables (AutoLISP)
guid: "GUID-529819FD-9E9C-4789-BA18-B981BA32DD23"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-529819FD-9E9C-4789-BA18-B981BA32DD23.htm"
generated: "2025-11-28T19:06:06.816561Z"
description: AutoLISP applications can inspect and change the value of AutoCAD system variables with the getvar and setvar functions.
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

# About System and Environment Variables (AutoLISP)

> AutoLISP applications can inspect and change the value of AutoCAD system variables with the getvar and setvar functions.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-529819FD-9E9C-4789-BA18-B981BA32DD23.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-529819FD-9E9C-4789-BA18-B981BA32DD23.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

These functions use a string to specify the variable name. The `setvar`  function requires a second argument that specifies the new value the system variable. AutoCAD system variables accept and return various data types: integers, reals, strings, 2D points, and 3D points.

Values supplied as arguments to `setvar`  must be of the expected type. If an invalid type is supplied, an AutoLISP error is generated.

The following example code demonstrates how to get and set the value of the AutoCAD FILLETRAD system variable:

```lisp
(if (< (getvar "filletrad") 1)
  (setvar "filletrad" 1)
)
```

Additional functions, `getenv`  and `setenv`, provide AutoLISP routines with access to the currently defined operating system environment variables. Unlike system variable names, environment variable names are case specific. For example, MaxHatch and MAXHATCH are not the same. When using the `setenv`  function, you always supply the new value as a string even if it might be a numeric value.

The following example code demonstrates how to set the MaxHatch environment variable:

```lisp
(setq curMaxHatch (getenv "MaxHatch"))
(prompt (strcat "\nCurrent value of MaxHatch: " curMaxHatch))
(setenv "MaxHatch" "50000")
(prompt (strcat "\nNew value of MaxHatch: " (getenv "MaxHatch")))
(setenv "MaxHatch" curMaxHatch)
```
