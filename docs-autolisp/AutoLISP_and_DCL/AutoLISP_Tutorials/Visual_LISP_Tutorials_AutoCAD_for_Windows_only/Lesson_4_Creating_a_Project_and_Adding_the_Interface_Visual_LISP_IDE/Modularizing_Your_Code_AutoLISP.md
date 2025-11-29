---
title: Modularizing Your Code (AutoLISP)
guid: "GUID-E412C0D3-72A2-4B87-BACE-BCE0E28E006B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E412C0D3-72A2-4B87-BACE-BCE0E28E006B.htm"
generated: "2025-11-28T19:06:59.849264Z"
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

# Modularizing Your Code (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E412C0D3-72A2-4B87-BACE-BCE0E28E006B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E412C0D3-72A2-4B87-BACE-BCE0E28E006B.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

As a result of the work you did in Lesson 3, your *gpmain.lsp*  file was getting rather large. This is not a problem for Visual LISP, but it is easier to maintain the code if you split things up into files containing logically related functions. It is also easier to debug your code. For example, if you have a single file with 150 functions, a single missing parenthesis can be difficult to find.

In the tutorial, the files will be organized as follows:

| Tutorial file organization |  |
| --- | --- |
| File name | Contents |
| gp-io.lsp | All input and output (I/O) functions) such as getting user input. Also contains the AutoLISP code required for the dialog box interface you will be adding. |
| utils.lsp | Includes all generic functions that can be used again on other projects. Also contains load-time initializations. |
| gpdraw.lsp | All drawing routines—the code that actually creates the AutoCAD entities. |
| gpmain.lsp | The basic C:GPath function. |

## To split gpmain.lsp into four files

1. Create a new file, then cut and paste the following functions from
   gpmain.lsp
    into the new file:
   - gp:getPointInput
   - gp:getDialogInput

   Save the new file in your working directory as *gp-io.lsp*.
2. Create a new file, then cut and paste the following functions from
   gpmain.lsp
    into the new file:
   - Degrees->Radians
   -  3Dpoint->2Dpoint
   - gp:list->variantArray

   Also, at the beginning of the file, insert the lines of code to establish ActiveX functionality (`vl-load-com`) and commit global variable assignment (`*ModelSpace*`).

   Save the file as *utils.lsp*.
3. Create a new file, then cut and paste the following function from
   gpmain.lsp
    into the new file:
   - gp:drawOutline

   Save this file as *gpdraw.lsp*.
4. After stripping the code out of
   gpmain.lsp
   , save it and check it. Only the original function,
   C:GPath
   , should remain in the file.

Your desktop is starting to get crowded. You can minimize any window within Visual LISP and it stays accessible. Click the Select Window button on the toolbar to choose a window from a list, or click Window from the Visual LISP menu and select a window to view.![](../../../../_assets/GUID_3971D4FA_ECD2_42CF_A230_55BB2F780E84-4ae93de4.png)
