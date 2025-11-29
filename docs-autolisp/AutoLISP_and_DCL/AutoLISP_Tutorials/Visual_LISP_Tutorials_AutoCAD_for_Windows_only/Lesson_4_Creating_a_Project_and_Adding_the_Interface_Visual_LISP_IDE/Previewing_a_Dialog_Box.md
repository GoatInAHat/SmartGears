---
title: Previewing a Dialog Box
guid: "GUID-F59EF490-CC98-45E8-92D4-CA12C286CBDF"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F59EF490-CC98-45E8-92D4-CA12C286CBDF.htm"
generated: "2025-11-28T19:07:00.478225Z"
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

# Previewing a Dialog Box

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F59EF490-CC98-45E8-92D4-CA12C286CBDF.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-F59EF490-CC98-45E8-92D4-CA12C286CBDF.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

Visual LISP provides a preview feature for checking the results of your DCL coding.

## To preview a dialog box defined with DCL

1.  Click Tools
    Interface Tools
    Preview DCL in Editor from the Visual LISP menu.
2. Click OK when prompted to specify a dialog name.

   In this case, your DCL file defines just a single dialog box, so there is no choice to be made. As you create larger and more robust applications, however, you may end up with DCL files containing multiple dialog boxes. This is where you can select which one to preview.
3. If the dialog box displays successfully, choose any button to end the dialog.

Visual LISP passes control to AutoCAD to display the dialog box. If AutoCAD finds syntactical errors, it displays one or more message windows identifying the errors.

If AutoCAD detects DCL errors and you are unable to figure out how to fix them, copy the *gpdialog.dcl*  file in your *Tutorial\VisualLISP\Lesson4*  directory and save it in the *Support*  directory.
