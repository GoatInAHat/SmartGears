---
title: "Updating a Stubbed-Out Function"
guid: "GUID-6BAB6CAD-3371-4965-8688-0DA37778CA2F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-6BAB6CAD-3371-4965-8688-0DA37778CA2F.htm"
generated: "2025-11-28T19:07:01.421173Z"
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

# Updating a Stubbed-Out Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-6BAB6CAD-3371-4965-8688-0DA37778CA2F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-6BAB6CAD-3371-4965-8688-0DA37778CA2F.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

You have now revised the `gp:getDialogInput`  function. Whenever you modify a stubbed-out function, you should always check a couple of things:

- Has the
  defun
   statement changed? That is, does the function still take the same number of arguments?
- Does the function return something different?

In the case of `gp:getDialogInput`, the answer to both questions is yes. The function now accepts the parameter of the path width (to set the default tile size and spacing). And instead of returning `T`, which is the value the stubbed-out version of the function returned, `gp:getDialogInput`  now returns an association list containing four new values.

Both changes affect the code that calls the function and the code that handles the return values from the functions. Replace your previous version of the `C:GPath`  function in *gpmain.lsp*  with the following code:

```lisp
(defun C:GPath (/ gp_PathData gp_dialogResults)
  ;; Ask the user for input: first for path location and
  ;; direction, then for path parameters.  Continue only if you
  ;; have valid input.  Store the data in gp_PathData.
  (if (setq gp_PathData (gp:getPointInput))
    (if (setq gp_dialogResults (gp:getDialogInput (cdr(assoc 40
                               gp_PathData))))
      (progn
        ;; Now take the results of gp:getPointInput and append this
        ;; to the added information supplied by gp:getDialogInput.
        (setq gp_PathData (append gp_PathData gp_DialogResults))

        ;; At this point, you have all the input from the user.
        ;; Draw the outline, storing the resulting polyline
        ;; "pointer" in the variable called PolylineName.
        (setq PolylineName (gp:drawOutline gp_PathData))
      ) ;_ end of progn
      (princ "\nFunction cancelled.")

    ) ;_ end of if
    (princ "\nIncomplete information to draw a boundary.")

  ) ;_ end of if
  (princ)  ; exit quietly

) ;_ end of defun
```

Take a look at the boldface lines in the revision of the main `C:GPath`  function. There are two essential changes to make the program work correctly:

-  When the
  gp:getDialogInput
   function is invoked, the path width is passed to it. This is done by extracting the value associated with the key 40 index of the
  gp_PathData
   association list.
-  The association list returned by
  gp:getPointInput
   is assigned to a variable called
  gp_dialogResults
  . If this variable has a value, its content needs to be appended to the association list values already stored in
  gp_PathData
  .

There are additional changes in the code resulting from the replacement of placeholders in the stubbed-out version. The easiest thing to do is copy this code from the online tutorial and paste it into your file.
