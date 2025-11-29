---
title: About Special Forms (AutoLISP)
guid: "GUID-F992EAA3-AFFC-425D-B7C9-6A9CB0EA2768"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F992EAA3-AFFC-425D-B7C9-6A9CB0EA2768.htm"
generated: "2025-11-28T19:06:05.499114Z"
description: Certain AutoLISP functions are considered special forms because they evaluate arguments in a different manner than most AutoLISP function calls.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# About Special Forms (AutoLISP)

> Certain AutoLISP functions are considered special forms because they evaluate arguments in a different manner than most AutoLISP function calls.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F992EAA3-AFFC-425D-B7C9-6A9CB0EA2768.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-F992EAA3-AFFC-425D-B7C9-6A9CB0EA2768.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

A typical function evaluates all arguments passed to it before acting on those arguments. Special forms either do not evaluate all their arguments, or only evaluate some arguments under certain conditions.

The following AutoLISP functions are considered special forms:

-  AND
-  COMMAND
-  COND
-  DEFUN
-  DEFUN-Q
-  FOREACH
-  FUNCTION
-  IF
-  LAMBDA
-  OR
-  PROGN
-  QUOTE
-  REPEAT
-  SETQ
-  TRACE
-  UNTRACE
-  VLAX-FOR
-  WHILE

Note:
 The
VLAX-FOR
 function is not available on Mac OS and Web.
