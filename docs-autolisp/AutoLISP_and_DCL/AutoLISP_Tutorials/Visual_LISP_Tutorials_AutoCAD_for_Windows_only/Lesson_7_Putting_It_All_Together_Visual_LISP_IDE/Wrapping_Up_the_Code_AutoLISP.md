---
title: Wrapping Up the Code (AutoLISP)
guid: "GUID-9FD1081D-CFD1-4EE6-BAD5-4B6540073770"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9FD1081D-CFD1-4EE6-BAD5-4B6540073770.htm"
generated: "2025-11-28T19:07:07.072513Z"
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

# Wrapping Up the Code (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9FD1081D-CFD1-4EE6-BAD5-4B6540073770.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9FD1081D-CFD1-4EE6-BAD5-4B6540073770.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

So far, you have done the following in this lesson:

- Modified the
  gp:drawOutline
   function so that it returns the polyline perimeter points in addition to the pointer to the polyline. You added this information to the
  gp_PathData
   variable. This variable is stored with the reactor data in the object reactor attached to every garden path.
- Updated the reactor functions in gpreact.lsp.
- Added functions
  xyzList->ListOfPoints
  ,
  xyList->ListOfPoints
  , and other utility functions to the
  utils.lsp
   file.
- Updated the
  gp:Calculate-and-Draw-Tiles
   function so that
  ObjectCreationStyle
   is now a parameter to the function rather than a local variable.
-  Modified the call to
  gp:Calculate-and-Draw-Tiles
   in the
  C:GPath
   function within the
  gpmain.lsp
   file.
-  Added
  gppoly.lsp
   to your project, and examined the functions within it.

Give the completed application a try. Save your work, then load in the project sources, run the `Gpath`  function, and try stretching and moving the garden path boundary. Remember: if something is not working and you are unable to debug the problem, you can load the completed code from the *Tutorial\VisualLISP\Lesson7*  directory.
