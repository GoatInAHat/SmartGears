---
title: Cleaning Up After Your Reactors
guid: "GUID-837583AE-15EB-4ACF-9F93-008020B06D5E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-837583AE-15EB-4ACF-9F93-008020B06D5E.htm"
generated: "2025-11-28T19:07:04.460502Z"
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

# Cleaning Up After Your Reactors

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-837583AE-15EB-4ACF-9F93-008020B06D5E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-837583AE-15EB-4ACF-9F93-008020B06D5E.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Reactors are indeed very active. When you design an application that relies on them, you could very well spend a great deal of time crashing your program and possibly crashing AutoCAD as well. It helps to have a tool available to remove all the reactors you have added, if necessary.

The *gpreact.lsp*  file includes a function `gp:clean-all-reactors`  that does not do much on its own. Instead, it makes a call to the `CleanReactors`  function. Add this function to your *utils.lsp*  file by copying the following code to the end of the file:

```lisp
;;;--------------------------------------------------------------;
;;;     Function: CleanReactors                                  ;
;;;--------------------------------------------------------------;
;;;  Description: General utility function used for cleaning up  ;
;;;               reactors. It can be used during debugging, as  ;
;;;               well as cleaning up any open reactors before   ;
;;;               a drawing is closed.                           ;
;;;--------------------------------------------------------------;
(defun CleanReactors ()
  (mapcar 'vlr-remove-all
         '(:VLR-AcDb-reactor
           :VLR-Editor-reactor
           :VLR-Linker-reactor
           :VLR-Object-reactor
          )
  )
)
```
