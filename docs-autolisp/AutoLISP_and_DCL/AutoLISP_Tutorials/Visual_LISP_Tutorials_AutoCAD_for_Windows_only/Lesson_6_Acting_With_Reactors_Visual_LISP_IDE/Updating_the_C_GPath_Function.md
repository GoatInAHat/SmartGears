---
title: "Updating the C:GPath Function"
guid: "GUID-E133F06A-FA83-4D38-B34F-EB7100F92ACC"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E133F06A-FA83-4D38-B34F-EB7100F92ACC.htm"
generated: "2025-11-28T19:07:04.239850Z"
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

# Updating the C:GPath Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E133F06A-FA83-4D38-B34F-EB7100F92ACC.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E133F06A-FA83-4D38-B34F-EB7100F92ACC.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

Update the `C:GPath`  function by adding reactor creation logic.

## To add reactor creation logic to C:GPath

1.  Replace your version of
   gpmain.lsp
    with the updated version shown below. Copy this code from the
    <AutoCAD directory>\Tutorial\VisualLISP\Lesson6
    directory:

   ```lisp
   (defun C:GPath (/
           gp_PathData
           gp_dialogResults
           PolylineName
           tileList
              )
     (setvar "OSMODE" 0)              ;; Turn off object snaps

     ;|
     ;; Lesson 6 adds a stubbed-out command reactor to AutoCAD
     ;; However, it would be undesirable to react to every
     ;; drawing of a circle should the COMMAND tile creation
     ;; method be chosen by the user.  So, disable the
     ;; *commandReactor* in case it exists.
     |;
     (if *commandReactor*
       (progn
         (setq *commandReactor* nil)
         (vlr-remove-all :VLR-Command-Reactor)
       )
     )

     ;; Ask the user for input: first for path location and
     ;; direction, then for path parameters.  Continue only if you
     ;; have valid input.  Store the data in gp_PathData.
     (if (setq gp_PathData (gp:getPointInput))
       (if (setq gp_dialogResults
              (gp:getDialogInput
            (cdr (assoc 40 gp_PathData))
              ) ;_ end of gp:getDialogInput
       ) ;_ end of setq

       (progn
         ;; Now take the results of gp:getPointInput and append this to
         ;; the added information supplied by gp:getDialogInput
         (setq gp_PathData (append gp_PathData gp_DialogResults))

         ;; At this point, you have all the input from the user
         ;; Draw the outline, storing the resulting polyline "pointer"
         ;; in the variable called PolylineName
         (setq PolylineName (gp:drawOutline gp_PathData))

         ;; Next, it is time to draw the tiles within the boundary.
         ;; The gp_tileList contains a list of the object pointers for
         ;; the tiles.  By counting up the number of points (using the
         ;; length function), we can print out the results of how many
         ;; tiles were drawn.
         (princ "\nThe path required ")
         (princ
           (length
             (setq tileList (gp:Calculate-and-Draw-Tiles gp_PathData))
           ) ;_ end of length
         ) ;_ end of princ
         (princ " tiles.")

         ;; Add the list of pointers to the tiles (returned by
         ;; gp:Calculate-and-Draw-Tiles) to gp_PathData. This will
         ;; be stored in the reactor data for the reactor attached
         ;; to the boundary polyline.  With this data, the polyline
         ;; "knows" what tiles (circles) belong to it.
         (setq gp_PathData
                (append (list (cons 100 tileList))
                         ; all the tiles
                    gp_PathData
                ) ;_ end of append
         ) ;_ end of setq

         ;; Before we attach reactor data to an object, let's look at
         ;; the function vlr-object-reactor
         ;; vlr-object-reactor has the following arguments:
         ;;  (vlr-object-reactor owner's data callbacks)
         ;;      The callbacks Argument is a list comprised
         ;;      '(event_name . callback_function)
         ;;
         ;; For this exercise we will use all arguments
         ;; associated with vlr-object-reactor
         ;; These reactor functions will execute only if
         ;; the polyline in PolylineName is modified or erased
         (vlr-object-reactor
           ;; The first argument for vlr-object-reactor is
           ;; the "Owner's List" argument.  This is where to
           ;; place the object to be associated with the
           ;; reactor.  In this case, it is the vlaObject
           ;; stored in PolylineName.
           (list PolylineName)

           ;; The second argument contains the data for the path
           gp_PathData

           ;; The third argument is the list of specific reactor
           ;; types that we are interested in using
           '
            (
             ;; reactor that is called upon modification of the object
             (:vlr-modified . gp:outline-changed)
             ;; reactor that is called upon erasure of the object
             (:vlr-erased . gp:outline-erased)
            )
         ) ;_ end of vlr-object-reactor
         ;; Next, register a command reactor to adjust the polyline
         ;; when the changing command is finished
         (if (not *commandReactor*)
           (setq *commandReactor*
             (VLR-Command-Reactor
                nil          ; No data is associated with the command reactor
                '(
                  (:vlr-commandWillStart . gp:command-will-start)
                  (:vlr-commandEnded . gp:command-ended)
                 )
              ) ;_ end of vlr-command-reactor
           )
         )

         ;; The following code removes all reactors when the drawing is
         ;; closed. This is extremely important!!!!!!!!!
         ;; Without this notification, AutoCAD may crash upon exiting!
         (if (not *DrawingReactor*)
              (setq *DrawingReactor*
              (VLR-DWG-Reactor
                   nil          ; No data is associated with the drawing reactor
                   '((:vlr-beginClose . gp:clean-all-reactors)
                    )
                 ) ;_ end of vlr-DWG-reactor
             )
           )
       ) ;_ end of progn
       (princ "\nFunction cancelled.")
     ) ;_ end of if

     (princ "\nIncomplete information to draw a boundary.")
     ) ;_ end of if

     (princ)               ; exit quietly
   ) ;_ end of defun

   ;;; Display a message to let the user know the command name.
   (princ "\nType GPATH to draw a garden path.")
   (princ)
   ```
2.  Review the code modifications and comments describing what each new statement does. This tutorial shows all modified code in boldface.
