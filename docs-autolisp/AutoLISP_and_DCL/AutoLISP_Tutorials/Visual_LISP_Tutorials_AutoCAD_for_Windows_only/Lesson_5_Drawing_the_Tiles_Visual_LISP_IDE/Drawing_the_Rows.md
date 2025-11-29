---
title: Drawing the Rows
guid: "GUID-7E4CD912-4CC0-45D0-B7A6-FBAA882C91E1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-7E4CD912-4CC0-45D0-B7A6-FBAA882C91E1.htm"
generated: "2025-11-28T19:07:03.047588Z"
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

# Drawing the Rows

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-7E4CD912-4CC0-45D0-B7A6-FBAA882C91E1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-7E4CD912-4CC0-45D0-B7A6-FBAA882C91E1.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

See if you can make sense of the following function. Compare it to the pseudo-code and try to catch the geometric calculations just described. There may be a few AutoLISP functions that are new to you, refer to the AutoLISP documentation if you need help with these functions. For now, just read the code; do not write anything.

```lisp
(defun gp:Calculate-and-Draw-Tiles (BoundaryData / PathLength
                                    TileSpace TileRadius SpaceFilled SpaceToFill
                                    RowSpacing offsetFromCenter
                                    rowStartPoint pathWidth pathAngle
                                    ObjectCreationStyle TileList)
  (setq PathLength          (cdr (assoc 41 BoundaryData))
        TileSpace           (cdr (assoc 43 BoundaryData))
        TileRadius          (cdr (assoc 42 BoundaryData))
        SpaceToFill         (- PathLength TileRadius)
        RowSpacing          (* (+ TileSpace (* TileRadius 2.0))
                              (sin (Degrees->Radians 60))
                            ) ;_ end of *
        SpaceFilled         RowSpacing
        offsetFromCenter    0.0
        offsetDistance      (/ (+ (* TileRadius 2.0) TileSpace) 2.0)
        rowStartPoint       (cdr (assoc 10 BoundaryData))
        pathWidth           (cdr (assoc 40 BoundaryData))
        pathAngle           (cdr (assoc 50 BoundaryData))
        ObjectCreationStyle (strcase (cdr (assoc 3 BoundaryData)))
  ) ;_ end of setq

  ;; Compensate for the first call to gp:calculate-Draw-tile Row
  ;; in the loop below.
  (setq rowStartPoint
        (polar rowStartPoint
               (+ pathAngle pi)
               (/ TileRadius 2.0)
        ) ;_ end of polar
  ) ;_ end of setq
  ;; Draw each row of tiles.
  (while (<= SpaceFilled SpaceToFill)
    ;; Get the list of tiles created, adding them to our list.
    (setq tileList
          (append tileList
                  (gp:calculate-Draw-TileRow
                    (setq rowStartPoint
                          (polar rowStartPoint
                                 pathAngle
                                 RowSpacing
                          ) ;_ end of polar
                    ) ;_ end of setq
                    TileRadius
                    TileSpace
                    pathWidth
                    pathAngle
                    offsetFromCenter
                    ObjectCreationStyle
                  ) ;_ end of gp:calculate-Draw-TileRow
          ) ;_ end of append
          ;; Calculate the distance along the path for the next row.
          SpaceFilled
          (+ SpaceFilled RowSpacing)
          ;; Alternate between a zero and a positive offset
          ;; (causes alternate rows to be indented).
          offsetFromCenter
          (if (= offsetFromCenter 0.0)
            offsetDistance
            0.0
          ) ;_ end of if
    ) ;_ end of setq
  ) ;_ end of while
  ;; Return the list of tiles created.
  tileList
) ;_ end of defun
```

A couple of sections from the code may need a little extra explanation.

The following code fragment occurs right before the `while`  loop begins:

```lisp
;; Compensate for the very first start point!!
(setq rowStartPoint(polar rowStartPoint
(+ pathAngle pi)(/ TileRadius 2.0)))
```

There are three pieces to the puzzle of figuring out the logic behind this algorithm:

-  The
  rowStartPoint
   variable starts its life within the
  gp:Calculate-and-Draw-Tiles
   function by being assigned the point the user selected as the start point of the path.
-  The very first argument passed to the
  gp:calculate-Draw-TileRow
   function does the following:

  `(setq rowStartPoint(polar rowStartPoint pathAngle RowSpacing))`

  Another way of stating this is: At the time the `gp:calculate-Draw-TileRow`  function is called, the `rowStartPoint`  variable is set to one `RowSpacing`  distance beyond the current `rowStartPoint`.
-  The
  rowStartPoint
   argument is used within
  gp:calculate-Draw-TileRow
   as the starting point for the centers of the circles in the row.

To compensate for the initial forward shifting of the `rowStartPoint`  during the drawing of the first row (that is, the first cycle through the `while`  loop), you will want to shift `rowStartPoint`  slightly in the opposite direction. The aim is to avoid the appearance of a large margin of empty space between the path boundary and the first row. Half the `TileRadius`  is a sufficient amount by which to move the point. This can be achieved by using `polar`  to project `rowStartPoint`  along a vector oriented 180 degrees from the `PathAngle`. If you think about it, this places the point temporarily outside the path boundary.

The next fragment (modified for readability) may be a little puzzling:

```lisp
(setq tileList
  (append tileList
          (gp:calculate-Draw-TileRow
            (setq rowStartPoint
                  (polar rowStartPoint pathAngle RowSpacing)
            ) ;_ end of setq
            TileRadius
            TileSpace
            pathWidth
            pathAngle
            offsetFromCenter
            ObjectCreationStyle
          )
  )
)
```

In essence, there is `setq`  wrapped around an `append`  wrapped around the call to `gp:calculate-Draw-TileRow`.

The `gp:calculate-Draw-TileRow`  function will return the Object IDs for each tile drawn. (The Object ID points to the tile object in the drawing.) You are drawing the tiles row by row, so the function returns the Object IDs of one row at a time. The `append`  function adds the new Object IDs to any existing Object IDs stored in `tileList`.

```lisp
; Near the end of the function, you can find the following code fragment:
(setq offsetFromCenter
  (if (= offsetFromCenter 0.0)
    offsetDistance
    0.0
  )
)
```

This is the offset toggle, which determines whether the row being drawn should begin with a circle centered on the path or offset from the path. The pseudo-code for this algorithm follows:

```lisp
; Set the offset amount to the following:
; If the offset is currently zero, set it to the offset distance;
; Otherwise, set it back to zero.
```
