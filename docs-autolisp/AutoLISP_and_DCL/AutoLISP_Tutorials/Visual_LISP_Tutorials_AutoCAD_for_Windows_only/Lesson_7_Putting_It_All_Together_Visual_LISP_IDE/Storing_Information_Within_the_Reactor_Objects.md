---
title: Storing Information Within the Reactor Objects
guid: "GUID-0AB3F77F-A1A3-444D-8EC4-5B99B0DBC855"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0AB3F77F-A1A3-444D-8EC4-5B99B0DBC855.htm"
generated: "2025-11-28T19:07:05.382777Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 04/02/2021
topic_subtype:
  - autolisp
---

# Storing Information Within the Reactor Objects

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0AB3F77F-A1A3-444D-8EC4-5B99B0DBC855.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0AB3F77F-A1A3-444D-8EC4-5B99B0DBC855.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 04/02/2021

One other important aspect of the application you need to think about is what kind of information to attach to the object reactor that is created for each polyline entity. In Lesson 6, you added code that attached the contents of `gp_PathData`  (the association list) to the reactor. You expanded the data carried within `gp_PathData`  by adding a new keyed field (100) to the association list. This new sublist is a list of pointers to all the circle entities assigned to a polyline boundary.

Because of the work that needs to be done to recalculate the polyline boundary, four additional key values should be added to `gp_pathData`:

```lisp
;;; StartingPoint                                                  ;
;;; (12 . BottomStartingPoint)      15------------------------14   ;
;;; (15 . TopStartingPoint)         |                          |   ;
;;; EndingPoint                     10    ----pathAngle--->   11   ;
;;; (13 . BottomEndingPoint)        |                          |   ;
;;; (14 . TopEndingPoint)           12------------------------13   ;
;;;                                                                ;
```

These ordered points are necessary to recalculate the polyline boundary whenever the user drags a corner grip to a new location. This information already exists within the `gp:drawOutline`  function in *gpdraw.lsp*. But look at the return value of the function. Currently, only the pointer to the polyline object is returned. So you need to do three things:

- Assemble the perimeter points in the format required.
- Modify the function so that it returns the perimeter point lists and the pointer to the polyline.
- Modify the
  C:GPath
   function so that it correctly deals with the new format of the values returned from
  gp:drawOutline
  .

Assembling the perimeter point lists is simple. Look at the code in `gp:drawOutline`. The local variable `p1`  corresponds to the key value 12, `p2`  to 13, `p3`  to 14, and `p4`  to 15. You can add the following function call to assemble this information:

```lisp
(setq polyPoints(list
  (cons 12 p1)
  (cons 13 p2)
  (cons 14 p3)
  (cons 15 p4)
))
```

Modifying the function so that it returns the polyline perimeter points and the polyline pointer is also easy. As the last expression within `gp:drawOutline`, assemble a list of the two items of information you want to return.

```lisp
(list pline polyPoints)
```

## To add program logic to save the polyline perimeter points

1.  Modify
   gp:drawOutline
    by making the changes shown in boldface in the following code (don't overlook the addition of the
   polyPoints
    local variable to the
   defun
    statement):

   ```lisp
   (defun gp:drawOutline (BoundaryData / PathAngle
                                         Width HalfWidth StartPt PathLength
                                         angm90 angp90 p1 p2
                                         p3 p4 poly2Dpoints
                                         poly3Dpoints plineStyle pline
                                         polyPoints
                         )
     ;; extract the values from the list BoundaryData.
     (setq PathAngle    (cdr (assoc 50 BoundaryData))
           Width        (cdr (assoc 40 BoundaryData))
           HalfWidth    (/ Width 2.00)
           StartPt      (cdr (assoc 10 BoundaryData))
           PathLength   (cdr (assoc 41 BoundaryData))
           angp90       (+ PathAngle (Degrees->Radians 90))
           angm90       (- PathAngle (Degrees->Radians 90))
           p1           (polar StartPt angm90 HalfWidth)
           p2           (polar p1 PathAngle PathLength)
           p3           (polar p2 angp90 Width)
           p4           (polar p3 (+ PathAngle
                          (Degrees->Radians 180)) PathLength)
           poly2Dpoints (apply 'append
                          (mapcar '3dPoint->2dPoint (list p1 p2 p3 p4))
                        )
           poly3Dpoints (mapcar 'float (append p1 p2 p3 p4))

           ;; get the polyline style.
           plineStyle   (strcase (cdr (assoc 4 BoundaryData)))

           ;; Add polyline to the model space using ActiveX automation.
           pline        (if (= plineStyle "LIGHT")
                          ;; create a lightweight polyline.
                          (vla-addLightweightPolyline
                            *ModelSpace*   ; Global Definition for Model Space
                            (gp:list->variantArray poly2Dpoints)
                            ;data conversion
                          ) ;_ end of vla-addLightweightPolyline
                          ;; or create a regular polyline.
                          (vla-addPolyline
                            *ModelSpace*
                            (gp:list->variantArray poly3Dpoints)
                            ;data conversion
                          ) ;_ end of vla-addPolyline
                        ) ;_ end of if
           polyPoints   (list
                          (cons 12 p1)
                          (cons 13 p2)
                          (cons 14 p3)
                          (cons 15 p4)
                        )
     ) ;_ end of setq

     (vla-put-closed pline T)
     (list pline polyPoints)
   ) ;_ end of defun
   ```
2.  Modify the
   C:GPath
    function (in
   gpmain.lsp
   ). Look for the line of code that currently looks like this:

   ```lisp
   (setq PolylineName (gp:drawOutline gp_PathData))
   ```

   Change it so it appears as follows:

   ```lisp
   (setq PolylineList     (gp:drawOutline gp_PathData)
         PolylineName     (car PolylineList)
         gp_pathData      (append gp_pathData (cadr PolylineList))
    ) ;_ end of setq
   ```

   The `gp_PathData`  variable now carries all the information required by the reactor function.
3.  Add
   PolylineList
    to the local variables section of the
   C:GPath
    function definition.
