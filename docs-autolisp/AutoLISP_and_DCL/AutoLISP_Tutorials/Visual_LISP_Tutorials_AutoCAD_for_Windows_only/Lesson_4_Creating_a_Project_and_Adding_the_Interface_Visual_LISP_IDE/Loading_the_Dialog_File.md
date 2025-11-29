---
title: Loading the Dialog File
guid: "GUID-EE4302F0-607E-407D-8A5F-3D388FC9DB5A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EE4302F0-607E-407D-8A5F-3D388FC9DB5A.htm"
generated: "2025-11-28T19:07:00.735925Z"
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

# Loading the Dialog File

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EE4302F0-607E-407D-8A5F-3D388FC9DB5A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EE4302F0-607E-407D-8A5F-3D388FC9DB5A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

Your program first needs to load the DCL file using the `load_dialog`  function. This function searches for dialog files according to the AutoCAD support file search path, unless you specify a full path name.

For every `load_dialog`  function there should be a corresponding `unload_dialog`  function later in the code. You will see this in a moment. For now, take a look at how you need to load in your dialog:

```lisp
;; Load the dialog box.  Set up error checking to make sure
;; the dialog file is loaded before continuing
(if (= -1 (setq dcl_id (load_dialog "gpdialog.dcl")))
    (progn
      ;; There's a problem - display a message and set the
      ;; dialogLoaded flag to nil
      (princ "\nCannot load gpdialog.dcl")
      (setq dialogLoaded nil)
    ) ;_ end of progn
) ;_ end of if
```

The `dialogLoaded`  variable indicates if the dialog loaded successfully. In the code where you set up the initial values for the dialog box, you set `dialogLoaded`  to an initial value of `T`. As you can see in the code fragment above, `dialogLoaded`  is set to `nil`  if there is a problem with the load.
