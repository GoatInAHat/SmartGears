---
title: About Exiting a Function Quietly (AutoLISP)
guid: "GUID-58A6EE05-20AD-4F5E-A067-1E891C99E726"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-58A6EE05-20AD-4F5E-A067-1E891C99E726.htm"
generated: "2025-11-28T19:06:03.322686Z"
description: If you invoke the princ function without passing an expression to it, it displays nothing and has no value to return.
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

# About Exiting a Function Quietly (AutoLISP)

> If you invoke the princ function without passing an expression to it, it displays nothing and has no value to return.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-58A6EE05-20AD-4F5E-A067-1E891C99E726.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-58A6EE05-20AD-4F5E-A067-1E891C99E726.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

So if you add a call to `princ`  without any arguments, after an expression, there is no return value. This is a great way to suppress the `nil`  that is often returned by the last expression within a custom function. This practice is called exiting quietly.
