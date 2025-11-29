---
title: Putting the Code Together
guid: "GUID-AF12AC1D-2F15-4D3D-BA0B-82D79C8A333D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-AF12AC1D-2F15-4D3D-BA0B-82D79C8A333D.htm"
generated: "2025-11-28T19:07:01.324761Z"
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

# Putting the Code Together

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-AF12AC1D-2F15-4D3D-BA0B-82D79C8A333D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-AF12AC1D-2F15-4D3D-BA0B-82D79C8A333D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

With the examples above, and a few additional lines, you have the code needed to complete the `gp:getDialogInput`  function.

## To put gp:getDialogInput together

1. Open your copy of
   gp-io.lsp
    in a Visual LISP text editor window.
2. Delete the code in
   gp:getDialogInput
    (the
   defun gp:getDialogInput
    statement and everything after it).
3. Enter the following
   defun
    statement as the first line of code in the
   gp:getDialogInput
    function:

   ```lisp
   (defun gp:getDialogInput (pathWidth / dcl_id objectCreateMethod
                                         plineStyle tilerad tilespace result UserClick
                                         dialogLoaded dialogShow)
   ```

   The function expects a single argument (`pathwidth`), and establishes a number of local variables.
4. Following the code you added in step 3, enter the sample code from each of the following sections:
   -  Setting Up Dialog Values
   -  Loading the Dialog File
   -  Loading a Specific Dialog into Memory
   -  Initializing the Default Dialog Values
   -  Assigning Actions to Tiles

   Note:
    Enter just the first code example from Assigning Actions to Tiles not the fragments in the explanations that follow. Those fragments just repeat pieces of the example.

   -  Starting the Dialog
   -  Unloading the Dialog
   -  Determining What to Do Next
5. After the last line of code, add the following:

   ```lisp
   )
    )
     Result;
   ) ;_ end of defun
   ```
6. Format the code you entered by clicking Tools
    Format Code in Editor from the Visual LISP menu.
