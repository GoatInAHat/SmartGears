---
title: "Understanding the Gp:FindMovedPoint Function"
guid: "GUID-2DAA4B0F-C8F8-4AD1-8B67-9679BA800AF3"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-2DAA4B0F-C8F8-4AD1-8B67-9679BA800AF3.htm"
generated: "2025-11-28T19:07:06.687256Z"
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

# Understanding the Gp:FindMovedPoint Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-2DAA4B0F-C8F8-4AD1-8B67-9679BA800AF3.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-2DAA4B0F-C8F8-4AD1-8B67-9679BA800AF3.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The `gp:FindMovedPoint`  function contains some very powerful LISP expressions dealing with list manipulation. Essentially, what this function does is compare the list of the current polyline points (after the user dragged one to a new location) to the previous points, and return the keyed list (the 13 <xvalue> <yvalue>) for the moved point.

The best way to figure out how this function works is to step through the code and watch the values that it manipulates. Set a breakpoint right at the first expression `(setq result . . .)`  and watch the following variables while you step through the function:

- KeyListToLookFor
- PresentPoints
- KeyedList
- Result
- KeyListStatus
- MissingKey
- MovedPoint

The `mapcar`  and `lambda`  functions will be examined in the following section. For now, however, follow the comments in the code to see if you can understand what is happening within the functions.
