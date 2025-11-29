---
title: Applying Some Geometry
guid: "GUID-F2AF3EC4-D6DA-47EC-9DA9-23B93304E1D0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F2AF3EC4-D6DA-47EC-9DA9-23B93304E1D0.htm"
generated: "2025-11-28T19:07:02.954436Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# Applying Some Geometry

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F2AF3EC4-D6DA-47EC-9DA9-23B93304E1D0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F2AF3EC4-D6DA-47EC-9DA9-23B93304E1D0.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

There are only a few dimensions you need to know to draw the garden path. The half-width is easy: it is just half the width of the path. You already defined the code to obtain this width from users and saved it in an association list.

Tile spacing is also easy; it is twice the radius (that is, the diameter) plus the space between the tiles. The dimensions are also obtained from users.

Row spacing is a little trickier, unless you are really sharp with trigonometry. Here is the formula:

```lisp
Row Spacing = (Tile Diameter + Space between Tiles) * (the sine
               of 60 degrees)
```
