---
title: Creating Entities Using ActiveX Functions
guid: "GUID-BC26675C-A124-4612-81AB-3087AE06F5AC"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BC26675C-A124-4612-81AB-3087AE06F5AC.htm"
generated: "2025-11-28T19:06:57.646721Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Creating Entities Using ActiveX Functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BC26675C-A124-4612-81AB-3087AE06F5AC.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-BC26675C-A124-4612-81AB-3087AE06F5AC.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The newest way of creating entities is by using the ActiveX functions within Visual LISP. ActiveX has several advantages over `entmake`  and `command`.

- ActiveX functions are faster.
- ActiveX function names indicate the action they perform, resulting in easier readability, maintenance, and bug-fixing.

You will see an example of an ActiveX function later in this lesson.
