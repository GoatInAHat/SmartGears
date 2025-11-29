---
title: Handling Nonlinear Reactor Sequences
guid: "GUID-A0D4D264-BA45-4600-B7E1-D09ADAA1A1B5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A0D4D264-BA45-4600-B7E1-D09ADAA1A1B5.htm"
generated: "2025-11-28T19:07:05.899619Z"
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

# Handling Nonlinear Reactor Sequences

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A0D4D264-BA45-4600-B7E1-D09ADAA1A1B5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-A0D4D264-BA45-4600-B7E1-D09ADAA1A1B5.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The final important detail deals with a quirk in the command/reactor sequence in AutoCAD when users modify a polyline by using the specialized GRIP commands. These commands, such as GRIP_MOVE and GRIP_ROTATE, are available from a shortcut menu after you select the grip of an object and right-click. The reactor sequence is not as linear as a simple MOVE or ERASE command. In effect, the user is changing to a different command while in the midst of another. To demonstrate this situation, you can load the code from Lesson 6 that traces the sequence of reactor events. Or simply review the following annotated Visual LISP Console window output to see what happens:

```lisp
;; To start, select the polyline and some of the circles by using a
;; crossing selection box. The items in the selection set--
;; the chosen circles and the polyline--are now shown with grips on.
;; To initiate the sequence, click on one of the polyline grips:
(GP:COMMAND-WILL-START #<VLR-Command-reactor> (GRIP_STRETCH))

;; Now change the command to a move by right-clicking and choosing
;; MOVE from the pop-up menu.  Notice that the command-ended
;; reactor fires in order to close out the GRIP_STRETCH command
;; without having fired an object reactor event:
(GP:COMMAND-ENDED #<VLR-Command-reactor> (GRIP_STRETCH))
(GP:COMMAND-WILL-START #<VLR-Command-reactor> (GRIP_MOVE))

;; Now drag the outline (and the selected circles) to a new location.
(GP:OUTLINE-CHANGED #<VLA-OBJECT IAcadLWPolyline 028f3188>
                 #<VLR-Object-reactor> nil)
(GP:COMMAND-ENDED #<VLR-Command-reactor> (GRIP_MOVE))
```

This demonstrates that you cannot be certain your object reactor callbacks will be called in all cases.

There is a related quirk in this sequence. Even during the final command-ended callback, the circles that are still part of the grip selection set cannot be deleted. These circles are still open by AutoCAD. If you attempt to erase them during the command-ended callback, you can crash AutoCAD. To get around this, you can use another global variable to store a list of pointers to the tile objects until they can be deleted.

## To process nonlinear reactor sequences

1.  Add the following function to the
   gpreact.lsp
    file:

   ```lisp
   (defun gp:erase-tiles (reactor / reactorData tiles tile)
     (if (setq reactorData (vlr-data reactor))
       (progn
         ;; Tiles in the path are stored as data in the reactor.
         (setq tiles (cdr (assoc 100 reactorData)))
         ;; Erase all the existing tiles in the path.
         (foreach tile tiles
             (if (and (null (member tile *Safe-to-Delete*))
                    (not (vlax-erased-p tile))
                    )
                 (progn
                    (vla-put-visible tile 0)
                    (setq *Safe-to-Delete* (cons tile *Safe-to-Delete*))
                 )
             )
         )
         (vlr-data-set reactor nil)
         )
       )
    )
   ```

   This new function will be used in the first phase of erasing tiles. Notice that the tiles are not actually erased: they are made invisible and are added to a global variable named `*Safe-to-Delete*`.
2.  Add the following function to the
   gpreact.lsp
    file:

   ```lisp
   (defun Gp:Safe-Delete (activeCommand)
     (if (not (equal
         (strcase (substr activeCommand 1 5))
           "GRIP_"
         )
     )
     (progn
        (if *Safe-to-Delete*
           (foreach Item *Safe-to-Delete*
             (if (not (vlax-erased-p Item))
               (vla-erase item)
             )
           )
         )
         (setq *Safe-to-Delete* nil)
         )
       )
    )
   ```

   This function can be invoked at a time when a GRIP_MOVE or GRIP_STRETCH command is not being executed.
