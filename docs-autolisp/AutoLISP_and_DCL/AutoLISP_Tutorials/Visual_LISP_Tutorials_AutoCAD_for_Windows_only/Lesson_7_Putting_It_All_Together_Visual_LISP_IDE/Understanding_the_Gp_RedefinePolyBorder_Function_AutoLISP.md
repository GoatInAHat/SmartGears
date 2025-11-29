---
title: "Understanding the Gp:RedefinePolyBorder Function (AutoLISP)"
guid: "GUID-8C05B587-C5E8-47CA-83B2-B9B0471BA559"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8C05B587-C5E8-47CA-83B2-B9B0471BA559.htm"
generated: "2025-11-28T19:07:06.614473Z"
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

# Understanding the Gp:RedefinePolyBorder Function (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8C05B587-C5E8-47CA-83B2-B9B0471BA559.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8C05B587-C5E8-47CA-83B2-B9B0471BA559.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The following pseudo-code shows the logic behind the main function, `gp:RedefinePolyBorder`:

```lisp
Function gp:RedefinePolyBorder
   Extract the previous polyline corner points (12, 13, 14, and 15
     key values).
   Find the moved corner point by comparing the previous
     polyline corner points with the current corner points.
     (The one "misfit" point will be the point that moved.)
   Set the new corner points by recalculating the two points
     adjacent to the moved point.
   Update the new corner points in the reactor data (that will
     be stored back in the reactor for the modified polyline).
   Update other information in the reactor data. (Start point,
      endpoint, width, and length of path need to be recalculated.)
End Function
```
