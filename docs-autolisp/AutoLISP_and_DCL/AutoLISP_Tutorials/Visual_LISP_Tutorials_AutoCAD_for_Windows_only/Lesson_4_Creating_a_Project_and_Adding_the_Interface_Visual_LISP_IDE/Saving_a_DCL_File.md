---
title: Saving a DCL File
guid: "GUID-9D2DB6F3-E90F-4DA9-B58F-9858BC53E396"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9D2DB6F3-E90F-4DA9-B58F-9858BC53E396.htm"
generated: "2025-11-28T19:07:00.373591Z"
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

# Saving a DCL File

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9D2DB6F3-E90F-4DA9-B58F-9858BC53E396.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9D2DB6F3-E90F-4DA9-B58F-9858BC53E396.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Before saving the file containing your DCL, consider the fact that AutoCAD ®  must be able to locate your DCL file during runtime. For this reason, the file must be placed in one of the AutoCAD Support File Search Path locations. (If you are unsure about these locations, click Application menu ![](../../../../_assets/ac_menuaro-f6601f42.gif)  Options from AutoCAD and examine the Support File Search Path locations under the Files tab.)

For now, you can save the file in the AutoCAD *Support*  directory.

## To save your DCL file

1.  Click File
    Save As from the Visual LISP menu.
2. In the Save As Type field of the Save As dialog box, choose DCL Source Files from the pull-down menu.
3. Change the Save In path to
   <AutoCAD directory>\Support
   .
4.  Enter the file name
   gpdialog.dcl
   .
5. Click Save.

Notice Visual LISP changes the syntax coloring scheme after you save the file. Visual LISP is designed to recognize DCL files and highlight the different types of syntactical elements.
