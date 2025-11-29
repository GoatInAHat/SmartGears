---
title: Loading a Specific Dialog Into Memory
guid: "GUID-E309CA7A-8BEF-4F2E-B650-692B7A0600EF"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E309CA7A-8BEF-4F2E-B650-692B7A0600EF.htm"
generated: "2025-11-28T19:07:00.816735Z"
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

# Loading a Specific Dialog Into Memory

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E309CA7A-8BEF-4F2E-B650-692B7A0600EF.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E309CA7A-8BEF-4F2E-B650-692B7A0600EF.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

It was noted earlier that a single DCL file may contain multiple dialog box definitions. The next step in using a dialog is to specify which dialog box definition to display. The following code demonstrates this:

```lisp
(if (and dialogLoaded
        (not (new_dialog "gp_mainDialog" dcl_id))
     ) ;_ end of and
    (progn
      ;; There's a problem...
      (princ "\nCannot show dialog gp_mainDialog")
      (setq dialogShow nil)
    ) ;_ end of progn
) ;_ end of if
```

Notice how the `and`  function is used to test if the dialog was loaded and if the call to `new_dialog`  was successful. If there are multiple expressions evaluated within an `**and**`  function call, evaluation of subsequent expressions is terminated with the first expression that evaluates to `nil`. In this case, if the `dialogLoaded`  flag is `nil`  (meaning the load function in the previous section failed), Visual LISP does not attempt to perform the `new_dialog`  function.

Notice that the code also accounts for the possibility that something might not be working properly with the DCL file, and sets the `dialogShow`  variable to `nil`  if that is the case.

The `new_dialog`  function simply loads the dialog into memory—it does not display it. The `start_dialog`  function displays the dialog box. All dialog box initialization, such as setting tile values, creating images or lists for list boxes, and associating actions with specific tiles must take place after the `new_dialog`  call and before the `start_dialog`  call.
