---
title: "Lesson 3: Drawing the Path Boundary (Visual LISP IDE)"
guid: "GUID-D0CF600F-566E-4D79-9DEB-3039A97E5348"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D0CF600F-566E-4D79-9DEB-3039A97E5348.htm"
generated: "2025-11-28T19:06:57.183230Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# Lesson 3: Drawing the Path Boundary (Visual LISP IDE)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D0CF600F-566E-4D79-9DEB-3039A97E5348.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D0CF600F-566E-4D79-9DEB-3039A97E5348.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 29/03/2023

Attention:
 This lesson requires the Visual LISP Editor and applies to AutoCAD for Windows only.

In this lesson, you will expand your program so it actually draws something within AutoCAD—the polyline outline of the garden path. To draw the border, you must create some utility functions that are not specific to a single application but are general in nature and may be recycled for later use. You will also learn about writing functions that accept arguments—data that is passed to the function from the outside—and why the use of arguments is a powerful programming concept. By the end of the lesson, you will draw an AutoCAD shape parametrically, which means dynamically drawing a shape based on the unique data parameters provided by the user.

## Topics in this Tutorial
