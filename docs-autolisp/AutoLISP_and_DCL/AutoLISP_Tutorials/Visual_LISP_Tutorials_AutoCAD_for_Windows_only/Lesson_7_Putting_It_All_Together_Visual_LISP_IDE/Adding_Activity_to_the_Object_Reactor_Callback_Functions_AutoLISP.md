---
title: Adding Activity to the Object Reactor Callback Functions (AutoLISP)
guid: "GUID-86A64396-216A-4F40-9E7A-07811BE4FB04"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-86A64396-216A-4F40-9E7A-07811BE4FB04.htm"
generated: "2025-11-28T19:07:05.546031Z"
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

# Adding Activity to the Object Reactor Callback Functions (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-86A64396-216A-4F40-9E7A-07811BE4FB04.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-86A64396-216A-4F40-9E7A-07811BE4FB04.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

In Lesson 6, you registered two callback functions with object reactor events. The `gp:outline-erased`  function was associated with the `:vlr-erased`  reactor event, and `gp:outline-changed`  was associated with the `:vlr-modified`  event. You need to make these functions do what they are intended to do.

## To make the object reactor callback functions do what they are intended to do

1.  In
   gpreact.lsp
   , change
   gp:outline-erased
    so it appears as follows:

   ```lisp
   (defun gp:outline-erased (outlinePoly reactor parameterList)
     (setq *reactorsToRemove*
          (cons reactor *reactorsToRemove*))
     (princ)
   ) ;_ end of defun
   ```

   There is just one operation performed here. The reactor attached to the polyline is saved to a list of all reactors that need to be removed. (Remember: though reactors are attached to entities, they are separate objects entirely, and their relationships to entities need to be managed just as carefully as regular AutoCAD entities.)
2. Change
   gp:outline-changed
    to reflect the following code:

   ```lisp
   (defun gp:outline-changed (outlinePoly reactor parameterList)
     (if *lostAssociativity*
       (setq *reactorsToRemove*
                                (cons reactor *reactorsToRemove*))
       (setq *polytochange*     outlinePoly
             *reactorsToChange* (cons reactor *reactorsToChange*))
       )
     (princ)
   )
   ```

   There are two categories of functions that can modify the polyline outline. The first category contains those commands that will break the path's associativity with its tiles. You checked for this condition in `gp:command-will-start`  and set the `*lostAssociativity*`  global variable accordingly. In this case, the tiles need to be erased, and the path is then in the user's hands. The other category is the grip mode of the STRETCH command, where associativity is retained and you need to straighten out the outline after the user has finished dragging a vertex to a new location.

   The `*polyToChange*`  variable stores a VLA-Object pointer to the polyline itself. This will be used in the `gp:command-ended`  function when it comes time to recalculate the polyline border.
