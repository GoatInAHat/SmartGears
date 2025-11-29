---
title: "Understanding the Gp:pointEqual, Gp:rtos2, and Gp:zeroSmallNum Functions"
guid: "GUID-E5929616-3181-4D31-A576-16B6125F6135"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E5929616-3181-4D31-A576-16B6125F6135.htm"
generated: "2025-11-28T19:07:06.963010Z"
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

# Understanding the Gp:pointEqual, Gp:rtos2, and Gp:zeroSmallNum Functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E5929616-3181-4D31-A576-16B6125F6135.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E5929616-3181-4D31-A576-16B6125F6135.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

These three functions are required to get around one of the quirks of programming in an AutoCAD system, which, as you are well aware, allows you a great deal of precision. Occasionally, though, numbers are not quite precise enough, due to the rounding up or down of floating point values defining geometric positions. You must be able to compare one set of points with other points, so you must deal with these cases.

Have you ever noticed that occasionally, when you list the information associated with an AutoCAD entity, you see a value such as `1.0e-017`? This number is *almost*  zero, but when you are comparing it to zero within a LISP program, *almost*  does not count.

Within the garden path, you need to be able to compare numbers without having to worry about the fact that `1.0e-017`  is not quite zero. The `gp:pointEqual`, `gp:rtos2`, and `gp:zeroSmallNum`  functions handle any discrepancies in rounding when comparing point lists.

This completes your tour of the functions in *gppoly.lsp*.
