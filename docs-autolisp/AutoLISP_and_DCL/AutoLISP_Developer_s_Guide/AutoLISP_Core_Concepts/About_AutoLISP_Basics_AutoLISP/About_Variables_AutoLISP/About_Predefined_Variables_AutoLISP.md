---
title: About Predefined Variables (AutoLISP)
guid: "GUID-195BD573-AD4A-4A40-B385-74FB291E218C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-195BD573-AD4A-4A40-B385-74FB291E218C.htm"
generated: "2025-11-28T19:06:02.800265Z"
description: AutoLISP has several predefined variables that can be used with your custom functions and commands.
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

# About Predefined Variables (AutoLISP)

> AutoLISP has several predefined variables that can be used with your custom functions and commands.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-195BD573-AD4A-4A40-B385-74FB291E218C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-195BD573-AD4A-4A40-B385-74FB291E218C.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

You can change the value of these variables with the `setq`  function. However, other applications might rely on their values being consistent; therefore, it is recommended that you do not modify these variables.

The following variables are predefined for use with AutoLISP applications:

- PAUSE
   - Defined as a constant string of a double backslash (\\) character. This variable is used with the command function to pause for user input.
- PI
   - Defined as the constant p (pi). It evaluates to approximately 3.14159.
- T
   - Defined as the constant
  T
  . This is used as a non-
  nil
   value.

Note:
 Visual LISP, by default, protects these variables from redefinition. You can override this protection through the Visual LISP Symbol Service feature or by setting a Visual LISP environment option. The Visual LISP IDE is not available in AutoCAD LT for Windows and on Mac OS.
