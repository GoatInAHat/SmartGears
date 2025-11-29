---
title: "Understanding the Gp:recalcPolyCorners Function"
guid: "GUID-8A8B35C8-AE0C-4C32-A063-FBF718390C0D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8A8B35C8-AE0C-4C32-A063-FBF718390C0D.htm"
generated: "2025-11-28T19:07:06.865119Z"
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

# Understanding the Gp:recalcPolyCorners Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8A8B35C8-AE0C-4C32-A063-FBF718390C0D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8A8B35C8-AE0C-4C32-A063-FBF718390C0D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The key to understanding how `gp:recalcPolyCorners`  works is to revisit the diagram showing what the key values of 12 through 15 represent:

In the diagram, the user moved the corner point associated with the key value of 14. This means the corner points associated with 13 and 15 need to be recalculated.

Point 15 needs to be moved along the current vector defined by point 12 to point 15 until it lines up with the new point 14. The vectors from 12 to 15, and from 14 to 15, must be perpendicular to each other. The same operation must be applied to recalculate the new location for point 13.

Now take another look at the code to see if it makes sense.
