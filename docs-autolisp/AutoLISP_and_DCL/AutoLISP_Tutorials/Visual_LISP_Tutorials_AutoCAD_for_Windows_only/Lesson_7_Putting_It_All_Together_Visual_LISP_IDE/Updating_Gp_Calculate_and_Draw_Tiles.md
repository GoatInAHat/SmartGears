---
title: "Updating Gp:Calculate-and-Draw-Tiles"
guid: "GUID-EB766BFB-9F93-4F71-AA13-BC8B6D760D09"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EB766BFB-9F93-4F71-AA13-BC8B6D760D09.htm"
generated: "2025-11-28T19:07:06.127989Z"
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

# Updating Gp:Calculate-and-Draw-Tiles

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EB766BFB-9F93-4F71-AA13-BC8B6D760D09.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-EB766BFB-9F93-4F71-AA13-BC8B6D760D09.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

Earlier in this lesson, it was noted that you need to force `gp:Calculate-and-Draw-Tiles`  to use ActiveX to create objects when invoked from a reactor callback. This means overriding the object creation style (ActiveX, `entmake`, or `command`) chosen by the user, if necessary. The code you just updated, in the `gp:command-ended`  function, contains the following invocation of the tile drawing routine:

```lisp
(setq tileList (gp:Calculate-and-Draw-Tiles
    ;; path data list without correct tile list.
    NewReactorData
    ;; Object creation function.
    ;; Within a reactor this *MUST* be ActiveX.
    "ActiveX"
   )
)
```

Two parameters are passed to `gp:Calculate-and-Draw-Tiles`: `NewReactorData`  (which is a list in the form of the original `gp_PathData`  association list) and the string `"ActiveX"`  (which will set the object creation style). But take a look at the current definition of `gp:Calculate-and-Draw-Tiles`. (In case you have forgotten, this function is defined in *gpdraw.lsp*.) Here is the part of the function that declares the parameters and local variables:

```lisp
(defun gp:Calculate-and-Draw-Tiles (BoundaryData /
                                    PathLength TileSpace
                                    TileRadius SpaceFilled
                                    SpaceToFill RowSpacing
                                    offsetFromCenter rowStartPoint
                                    pathWidth pathAngle
                                    ObjectCreationStyle TileList)
```

Notice only that one parameter is currently specified, and `ObjectCreationStyle`  is identified as a local variable. Review how the `ObjectCreationStyle`  variable is set, which is a little farther into the function:

```lisp
(setq ObjectCreationStyle (strcase (cdr (assoc 3 BoundaryData))))
```

The `ObjectCreationStyle`  is currently set internally within the function by retrieving the value tucked away in the `BoundaryData`  variable (the association list). But now you need to be able to override that value.

## To modify gp:Calculate-and-Draw-Tiles to accept an object creation style argument

1. Add the
   ObjectCreationStyle
    variable to the function argument.
2. Remove
   ObjectCreationStyle
    from the local variables.

   The `defun`  statement for the function should look like the following:

   ```lisp
   (defun gp:Calculate-and-Draw-Tiles (BoundaryData ObjectCreationStyle
                                       / PathLength TileSpace
                                       TileRadius SpaceFilled
                                       SpaceToFile RowSpacing
                                       offsetFromCenter rowStartPoint
                                       pathWidth pathAngle
                                       TileList) ; remove ObjectCreationStyle from locals
   ```

   Note that if you declare a variable both as a parameter (before the slash) and as a local variable (after the slash), Visual LISP will point this out to you. For example, if you declare `ObjectCreationStyle`  as both a parameter and a variable, then use the Visual LISP syntax checking tool on the `gp:Calculate-and-Draw-Tiles`  function, the following message will appear in the Build Output window:

   ```lisp
   ; *** WARNING: same symbol before and after / in arguments list: OBJECTCREATIONSTYLE
   ```
3.  Modify the first
   setq
    expression within
   gp:Calculate-and-Draw-Tiles
    so that it looks like the following:

   ```lisp
   (setq
      PathLength       (cdr (assoc 41 BoundaryData))
      TileSpace        (cdr (assoc 43 BoundaryData))
      TileRadius       (cdr (assoc 42 BoundaryData))
      SpaceToFill      (- PathLength TileRadius)
      RowSpacing       (* (+ TileSpace (* TileRadius 2.0))
                         (sin (Degrees->Radians 60))
                       )
      SpaceFilled      RowSpacing
      offsetFromCenter 0.0
      offsetDistance   (+ (* TileRadius 2.0) TileSpace) 2.0)
      rowStartPoint    (cdr (assoc 10 BoundaryData))
      pathWidth        (cdr (assoc 40 BoundaryData))
      pathAngle        (cdr (assoc 50 BoundaryData))
   ) ;_ end of setq

   (if (not ObjectCreationStyle)
     (setq ObjectCreationStyle (strcase (cdr (assoc 3 BoundaryData))))
   )
   ```

   The original assignment statement for `ObjectCreationStyle`  has been removed. The code now checks to see if a value has been provided for `ObjectCreationStyle`. If `ObjectCreationStyle`  is not set (that is, the value is `nil`), the function assigns it a value from the `BoundaryData`  variable.

   There is one more series of changes you need to make to `gp:Calculate-and-Draw-Tiles`.
