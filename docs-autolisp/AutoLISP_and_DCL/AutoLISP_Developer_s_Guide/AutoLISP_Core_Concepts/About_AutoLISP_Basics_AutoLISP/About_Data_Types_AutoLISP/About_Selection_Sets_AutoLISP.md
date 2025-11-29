---
title: About Selection Sets (AutoLISP)
guid: "GUID-E0CCD0D3-AB95-40CE-A5DD-8E9214DC5469"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-E0CCD0D3-AB95-40CE-A5DD-8E9214DC5469.htm"
generated: "2025-11-28T19:06:01.431932Z"
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

# About Selection Sets (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-E0CCD0D3-AB95-40CE-A5DD-8E9214DC5469.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-E0CCD0D3-AB95-40CE-A5DD-8E9214DC5469.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

Selection sets are groups of one or more objects (entities). You can interactively add objects to, or remove objects from, selection sets with AutoLISP routines.

The following example uses the `ssget`  function to return a selection set containing all the objects in a drawing.

```lisp
(ssget "X")

<Selection set: 1>
```
