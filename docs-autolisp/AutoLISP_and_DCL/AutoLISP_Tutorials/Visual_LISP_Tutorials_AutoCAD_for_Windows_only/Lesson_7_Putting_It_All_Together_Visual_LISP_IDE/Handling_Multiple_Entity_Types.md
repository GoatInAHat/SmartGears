---
title: Handling Multiple Entity Types
guid: "GUID-24284CD3-5B0B-48A5-8DDE-EA2C56D9D4C2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-24284CD3-5B0B-48A5-8DDE-EA2C56D9D4C2.htm"
generated: "2025-11-28T19:07:05.739557Z"
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

# Handling Multiple Entity Types

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-24284CD3-5B0B-48A5-8DDE-EA2C56D9D4C2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-24284CD3-5B0B-48A5-8DDE-EA2C56D9D4C2.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The first detail is that your application may draw two kinds of polylines: old-style and lightweight. These different polyline types return their entity data in different formats. The old-style polyline returns a list of twelve reals: four sets of *X*, *Y*, and *Z*  points. The lightweight polyline, though, returns a list eight reals: four sets of *X*  and *Y*  points.

You need to do some calculations to determine the revised polyline boundary after a user moves one of the vertices. It will be a lot easier to do the calculations if the polyline data has a consistent format.

The Lesson 7 version of the *utils.lsp*  file contains functions to perform the necessary format conversions: `xyzList->ListOfPoints`  extracts and formats 3D point lists into a list of lists, and `xyList->ListOfPoints`  extracts and formats 2D point lists into a list of lists.

## To add the code for converting polyline data into a consistent format

1.  If you have a copy of
   utils.lsp
    open in a Visual LISP text editor window, close it.
2. Copy the version of
   utils.lsp
    from the
   Tutorial\VisualLISP\Lesson7
    directory into your working directory.

   In addition to the two functions that reformat polyline data, *utils.lsp*  contains additional utility functions needed in handling user alterations to the garden path.
3.  Open
   utils.lsp
    in a VLISP text editor window and review the new code.
