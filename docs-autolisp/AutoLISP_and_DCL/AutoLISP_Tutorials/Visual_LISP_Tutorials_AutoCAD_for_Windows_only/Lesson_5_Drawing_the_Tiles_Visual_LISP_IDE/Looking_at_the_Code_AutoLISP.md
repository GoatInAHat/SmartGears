---
title: Looking at the Code (AutoLISP)
guid: "GUID-8B7A66F1-EB67-40BA-9756-63B3E1001003"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8B7A66F1-EB67-40BA-9756-63B3E1001003.htm"
generated: "2025-11-28T19:07:03.293068Z"
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

# Looking at the Code (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8B7A66F1-EB67-40BA-9756-63B3E1001003.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8B7A66F1-EB67-40BA-9756-63B3E1001003.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Now look at the code for the `gp:calculate-Draw-TileRow`  function:

```lisp
(defun gp:calculate-Draw-TileRow (startPoint TileRadius
                                  TileSpace pathWidth pathAngle
                                  offsetFromCenter ObjectCreationStyle /
                                  HalfWidth TileDiameter
                                  ObjectCreationFunction angp90 angm90
                                  firstCenterPt TileCenterPt TileList)
  (setq HalfWidth (- (/ pathWidth 2.00) TileRadius)
        Tilespacing (+ (* TileRadius 2.0) TileSpace)
        TileDiameter (* TileRadius 2.0)
        angp90 (+ PathAngle (Degrees->Radians 90))
        angm90 (- PathAngle (Degrees->Radians 90))
        firstCenterPt (polar startPoint angp90 offsetFromCenter)
        tileCenterPt firstCenterPt
        ObjectCreationStyle(strcase ObjectCreationStyle)
        ObjectCreationFunction
        (cond
          ((equal ObjectCreationStyle "ACTIVEX")
            gp:Create_activeX_Circle
          )
          ((equal ObjectCreationStyle "ENTMAKE")
            gp:Create_entmake_Circle
          )
          ((equal ObjectCreationStyle "COMMAND")
            gp:Create_command_Circle
          )
          (T
            (alert (strcat "ObjectCreationStyle in function gp:calculate-Draw-TileRow"
                           "\nis invalid. Contact developer for assistance."
                           "\n        ObjectCreationStyle set to ACTIVEX"
                   )
            )
            (setq ObjectCreationStyle "ACTIVEX")
          )
        )
  )

  ;; Draw the circles to the left of the center.
  (while (< (distance startPoint tileCenterPt) HalfWidth)
    ;; Add each tile to the list to return.
    (setq tileList
          (cons
            (ObjectCreationFunction tileCenterPt TileRadius)
            tileList
          )
    )
    ;; Calculate the center point for the next tile.
    (setq tileCenterPt
          (polar tileCenterPt angp90 TileSpacing)
    )
  );_ end of while

  ;; Draw the circles to the right of the center.
  (setq tileCenterPt
        (polar firstCenterPt angm90 TileSpacing)
  )
  (while (< (distance startPoint tileCenterPt) HalfWidth)
    ;; Add each tile to the list to return.
    (setq tileList
          (cons
            (ObjectCreationFunction tileCenterPt TileRadius)
            tileList
          )
    )
    ;; Calculate the center point for the next tile.
    (setq tileCenterPt
          (polar tileCenterPt angm90 TileSpacing)
    )
  );_ end of while

  ;; Return the list of tiles.
  tileList
) ;_ end of defun
```

The AutoLISP code logic follows the pseudo-code, with the following addition:

```lisp
(setq ObjectCreationFunction
  (cond
    ((equal ObjectCreationStyle "ACTIVEX")
       gp:Create_activeX_Circle
    )
    ((equal ObjectCreationStyle "ENTMAKE")
      gp:Create_entmake_Circle
    )
    ((equal ObjectCreationStyle "COMMAND")
      gp:Create_command_Circle
    )
    (T
      (alert
        (strcat
         "ObjectCreationStyle in function gp:calculate-Draw-TileRow"
         "\nis invalid.  Contact the developer for assistance."
         "\n        ObjectCreationStyle set to ACTIVEX"
        ) ;_ end of strcat
      ) ;_ end of alert
     (setq ObjectCreationStyle "ACTIVEX")
    )
  ) ;_ end of cond
) ;_ end of setq
```

Remember the specification to allow users to draw the tiles (circles) using either ActiveX, the `entmake`  function, or the `command`  function? The `ObjectCreationFunction`  variable is assigned one of three functions, depending on the `ObjectCreationStyle`  parameter (passed from `**C:GPath**`  and through `gp:Calculate-and-Draw-Tiles`). Here are the three functions as they will be defined in *gpdraw.lsp*:

```lisp
(defun gp:Create_activeX_Circle (center radius)
  (vla-addCircle *ModelSpace*
    (vlax-3d-point center) ; convert to ActiveX-compatible 3D point
    radius
  )
) ;_ end of defun
(defun gp:Create_entmake_Circle	(center radius)
  (entmake
    (list (cons 0 "CIRCLE") (cons 10 center) (cons 40 radius))
  )
  (vlax-ename->vla-object (entlast))
)
(defun gp:Create_command_Circle	(center radius)
  (command "._circle" center radius)
  (vlax-ename->vla-object (entlast))
)
```

The first function draws a circle using an ActiveX function and returns an ActiveX object.

The second function draws a circle using `entmake`. It returns an entity name converted into an ActiveX object.

The third function draws a circle using `command`. It also returns an entity name converted into an ActiveX object.
