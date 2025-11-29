---
title: "Modifying Other Calls to Gp:Calculate-and-Draw-Tiles"
guid: "GUID-47A2B1A5-5B06-4871-A38E-520F6ADB5DB0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-47A2B1A5-5B06-4871-A38E-520F6ADB5DB0.htm"
generated: "2025-11-28T19:07:06.270689Z"
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

# Modifying Other Calls to Gp:Calculate-and-Draw-Tiles

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-47A2B1A5-5B06-4871-A38E-520F6ADB5DB0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-47A2B1A5-5B06-4871-A38E-520F6ADB5DB0.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

In the reactor callback, a hard-coded string `"ActiveX"`  is passed to `gp:Calculate-and-Draw-Tiles`  as the `ObjectCreationStyle`  argument. But what about the other times `gp:Calculate-and-Draw-Tiles`  is invoked?

If you remember back to Lesson 4, it was pointed out that whenever you change a stubbed-out function, you need to ask the following questions:

- Has the function call (invocation) changed? That is, does the function still take the same number of arguments?
- Does the function return something different?

The same questions need to be asked any time you make a significant change to a working function as you build, refine, and update your applications. In this case, you need to find any other functions in your project that invoke `gp:Calculate-and-Draw-Tiles`. Visual LISP has a feature that helps you do this.

## To find all calls to gp:Calculate-and-Draw-Tiles in your project

1. In the Visual LISP text editor window, double-click on the word
   gp:Calculate-and-Draw-Tiles
    within the
   gpdraw.lsp
    file.
2.  Click Search
    Find from the Visual LISP menu.

   Because you preselected the function name, it is already listed as the string to search for.
3. Select the Project button listed under Search in the Find dialog box.

   When you select this option, the Find dialog box expands at the bottom, and you can select the project to be searched.
4. Specify your current project name, then click the Find button.

   Visual LISP displays the results in the Find output window:
5. Look at the results in the Find Output window and determine whether there are any other locations in your code where you make a call to
   gp:Calculate-and-Draw-Tiles
   . There should only be one: a location within
   gpmain.lsp
   .
6. In the Find Output window, double-click on the line of code calling
   gp:Calculate-and-Draw-Tiles
   .

   Visual LISP activates a text editor window and takes you right to that line of code in *gpmain.lsp*. The code currently appears as follows:

   ```lisp
   (setq tilelist (gp:Calculate-and-Draw-Tiles gp_PathData))
   ```
7. Replace the line of code with the following:

   ```lisp
   (setq tilelist (gp:Calculate-and-Draw-Tiles gp_PathData nil))
   ```

   Why `nil`? Take another look at the pseudo-code:

   ```lisp
   If ObjectCreationStyle is nil, assign it from the BoundaryData.
   ```

   Passing `nil`  as a parameter to `gp:Calculate-and-Draw-Tiles`  causes that function to check the user's choice of how to draw the tiles (as determined by the dialog box selection and stored in `gp_PathData`). Subsequent calls from the command-ended reactor callback, however, will override this behavior by forcing the use of ActiveX.

Congratulations! You now have the basic reactor functionality in place. If you prefer, copy the *gpmain.lsp*  and *gpdraw.lsp*  files from the *Tutorial\VisualLISP\Lesson7*  into your working directory and examine the completed, debugged code.

There is still a lot of work to be done, and it is all triggered from this fragment of code in the `gp:Command-ended`  function:

```lisp
(setq NewReactorData
    (gp:RedefinePolyBorder CurrentPoints reactorData)
) ;_ end of setq
```
