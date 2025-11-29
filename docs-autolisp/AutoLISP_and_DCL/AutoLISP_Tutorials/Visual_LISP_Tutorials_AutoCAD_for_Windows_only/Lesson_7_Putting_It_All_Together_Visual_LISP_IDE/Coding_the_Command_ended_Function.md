---
title: "Coding the Command-ended Function"
guid: "GUID-B01C9105-4ECD-4C3F-AED2-5B5A53966C61"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-B01C9105-4ECD-4C3F-AED2-5B5A53966C61.htm"
generated: "2025-11-28T19:07:05.969712Z"
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

# Coding the Command-ended Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-B01C9105-4ECD-4C3F-AED2-5B5A53966C61.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-B01C9105-4ECD-4C3F-AED2-5B5A53966C61.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Now that you have seen the pseudo-code and handled some important details, replace the stubbed-out code in the `gp:command-ended`  reactor callback with the following:

```lisp
(defun gp:command-ended (reactor command-list
                         / objReactor
                         reactorToChange reactorData
                         coordinateValues currentPoints
                         newReactorData newPts
                         tileList
                         )
  (cond
    ;; CONDITION 1 - POLYLINE ERASED (Erase command)
    ;; If one or more polyline borders are being erased (indicated
    ;; by the presence of *reactorsToRemove*), erase the tiles
    ;; within the border, then remove the reactor.
    (*reactorsToRemove*
      (foreach objReactor *reactorsToRemove*
        (gp:erase-tiles objReactor)
      )
      (setq *reactorsToRemove* nil)
    )

    ;; CONDITION 2 - LOST ASSOCIATIVITY (Move, Rotate, etc.)
    ;; If associativity has been lost (undo, move, etc.), then
    ;; erase the tiles within each border
    ;;
    ((and *lostassociativity* *reactorsToChange*)
      (foreach reactorToChange *reactorsToChange*
        (gp:erase-tiles reactorToChange)
      )
      (setq *reactorsToChange* nil)
    )

    ;; CONDITION 3 - GRIP_STRETCH
    ;; In this case, the associativity of the tiles to the path is
    ;; kept, but the path and the tiles will need to be
    ;; recalculated and redrawn. A GRIP_STRETCH can only be
    ;; performed on a single POLYLINE at a time.
    ((and (not *lostassociativity*)
      *polytochange*
      *reactorsToChange*
      (member "GRIP_STRETCH" command-list)
      ;; for a GRIP_STRETCH, there will be only one reactor in
      ;; the global *reactorsToChange*.
      (setq reactorData
         (vlr-data (setq reactorToChange
                  (car *reactorsToChange*)
                 )
               )
        )
     )

     ;; First, erase the tiles within the polyline border.
     (gp:erase-tiles reactorToChange)

     ;; Next, get the current coordinate values of the polyline
     ;; vertices.
     (setq coordinateValues
       (vlax-safearray->list
         (vlax-variant-value
           (vla-get-coordinates *polyToChange*)
         )
       )
     )

     ;; If the outline is a lightweight polyline, you have
     ;; 2d points, so use utility function xyList->ListOfPoints
     ;; to convert the coordinate data into lists of
     ;; ((x y) (x y) ...) points. Otherwise, use the
     ;; xyzList->ListOfPoints function that deals
     ;; with 3d points, and converts the coordinate data into
     ;; lists of ((x y z) (x y z) ... ) points.
     (setq CurrentPoints
        (if (= (vla-get-ObjectName *polytochange*) "AcDbPolyline")
          (xyList->ListOfPoints coordinateValues)
          (xyzList->ListOfPoints coordinateValues)
        )
     )

     ;; Send this new information to RedefinePolyBorder -- this
     ;; will return the new Polyline Border
     (setq NewReactorData
       (gp:RedefinePolyBorder CurrentPoints reactorData)
     )

     ;; Get all the border Points and ...
     (setq newpts
            (list
              (cdr (assoc 12 NewReactorData))
              (cdr (assoc 13 NewReactorData))
              (cdr (assoc 14 NewReactorData))
              (cdr (assoc 15 NewReactorData))
            )
     )

     ;; ...update the outline of the polyline with the new points
     ;; calculated above.  If dealing with a lightweight polyline,
     ;; convert these points to 2D (since all the points in
     ;; newpts are 3D), otherwise leave them alone.
     (if (= (cdr (assoc 4 NewReactorData)) "LIGHT")
       (setq newpts (mapcar '(lambda (point)
                        (3dPoint->2dPoint Point)
                      )
                      newpts
                    )
       )
     )

     ;; Now update the polyline with the correct points.
     (vla-put-coordinates
       *polytochange*

       ;; For description of the list->variantArray see utils.lsp.
       (gp:list->variantArray (apply 'append newpts))
     )

     ;; Now use the current definition of the NewReactorData,
     ;; which is really the same as the garden path data
     ;; structure. The only exception is that the field (100)
     ;; containing the list of tiles is nil.  This is OK since
     ;; gp:Calculate-and-Draw-Tiles does not require this field
     ;; to draw the tiles. In fact this function creates the tiles
     ;; and returns a list of drawn tiles.
     (setq tileList (gp:Calculate-and-Draw-Tiles
              ;; path data list without correct tile list
              NewReactorData
              ;; Object creation function
              ;; Within a reactor this *MUST* be ActiveX
              "ActiveX"
              )
     )

     ;; Now that you have received all the tiles drawn, rebuild
     ;; the data structure with the correct tileList value and
     ;; reset the data property in the reactor.
     ;; Update the tiles associated with the polyline border.
     (setq NewReactorData
        (subst (cons 100 tileList)
           (assoc 100 NewReactorData)
           NewReactorData
           )
     )

     ;; By now you have the new data associated with the polyline.
     ;; All there is left to do is associate it with the reactor
     ;; using vlr-data-set.
     (vlr-data-set (car *reactorsToChange*) NewReactorData)

     ;; Remove all references to the temporary
     ;; variables *polytochange* and *reactorsToChange*.
     (setq *polytochange*     nil
       *reactorsToChange* nil
       )
     )
    )
    ;; Delete any items in the *Safe-to-Delete* global if you can!!!
    (Gp:Safe-Delete (car command-list))
   (princ)
 )
```
