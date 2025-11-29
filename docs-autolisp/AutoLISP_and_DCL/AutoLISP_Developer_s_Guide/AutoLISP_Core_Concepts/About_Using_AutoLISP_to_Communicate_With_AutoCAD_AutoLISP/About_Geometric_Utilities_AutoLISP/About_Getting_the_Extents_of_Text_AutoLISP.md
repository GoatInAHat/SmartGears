---
title: About Getting the Extents of Text (AutoLISP)
guid: "GUID-DA97206B-85C2-4746-B0E6-67E8ABE99117"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-DA97206B-85C2-4746-B0E6-67E8ABE99117.htm"
generated: "2025-11-28T19:06:08.472041Z"
description: The textbox function returns the diagonal coordinates of a box that encloses text.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# About Getting the Extents of Text (AutoLISP)

> The textbox function returns the diagonal coordinates of a box that encloses text.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-DA97206B-85C2-4746-B0E6-67E8ABE99117.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-DA97206B-85C2-4746-B0E6-67E8ABE99117.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

The `textbox`  function takes an entity definition list of the type returned by `entget`  (an association list of group codes and values) as its single argument. This list can contain a complete association list description of the text object or just a list describing the text string.

The points returned by `textbox`  describe the bounding box (an imaginary box that encloses the text) of the text object, as if its insertion point were located at (0,0,0) and its rotation angle were 0. The first list returned is the point (0.0 0.0 0.0), unless the text object is oblique or vertical or it contains letters with descenders (such as g and p). The value of the first point list specifies the offset distance from the text insertion point to the lower-left corner of the smallest rectangle enclosing the text. The second point list specifies the upper-right corner of that box. The returned point lists always describe the bottom-left and upper-right corners of this bounding box, regardless of the orientation of the text being measured.

The following example code shows the minimum allowable entity definition list that `textbox`  accepts. Because no additional information is provided, `textbox`  uses the current defaults for text style and height.

Command: **(textbox '((1 . "Hello world")))**

((0.0 0.0 0.0) (2.80952 1.0 0.0))

The actual values returned by `textbox`  will vary depending on the current text style.

The following example code demonstrates one method of providing the `textbox`  function with an entity definition list.

Command: **text**

Specify start point of text or [Justify/Style]: **1,1**

Specify height <0.2000>: *Press Enter*

Specify rotation angle of text <0>: *Press Enter*

In the in-place text editor, enter **test**

In the in-place text editor, *Press Enter*

Command: **(setq e (entget (entlast)))**

((-1 . <Entity name: 7ffffb05da0>) (0 . "TEXT") (330 . <Entity name:

7ffffb039f0>) (5 . "1D2") (100 . "AcDbEntity") (67 . 0) (410 .

"Model") (8 . "0") (100 . "AcDbText") (10 1.0 1.0 0.0) (40 . 0.2) (1 .

"test") (50 . 0.0) (41 . 1.0) (51 . 0.0) (7 . "Standard") (71 . 0) (72

. 0) (11 0.0 0.0 0.0) (210 0.0 0.0 1.0) (100 . "AcDbText") (73 . 0))

Command: **(textbox e)**

((0.00491132 -0.00327422 0.0) (0.448295 0.195498 0.0))

The following figure shows the results of applying `textbox`  to a text object with a height of 1.0. The figure also shows the baseline and insertion point of the text.

If the text is vertical or rotated, `pt1`  is still the bottom-left corner and `pt2`  is the upper-right corner; the bottom-left point may have negative offsets if necessary.

The following figure shows the point values (`pt1`  and `pt2`) that `textbox`  returns for samples of vertical and aligned text. In both samples, the height of the letters is 1.0. (For the aligned text, the height is adjusted to fit the alignment points.)

When using vertical text styles, the points are still returned in left-to-right, bottom-to-top order as they are for horizontal styles, so that the first point list will contain negative offsets from the text insertion point.

Regardless of the text orientation or style, the points returned by `textbox`  are such that the text insertion point (group code 10) directly translates to the origin point of the object coordinate system (OCS) for the associated text object. This point can be referenced when translating the coordinates returned from `textbox`  into points that define the actual extent of the text. The following two sample functions use `textbox`  to place a box around selected text regardless of its orientation.

The first routine uses the `textbox`  function to draw a box around a selected text object:

```lisp
(defun C:TBOX ( / textent tb ll ur ul lr)
  (setq textent (car (entsel "\nSelect text: ")))
  (command "._ucs" "Object" textent)
  (setq tb (textbox (list (cons -1 textent)))
        ll (car tb)
        ur (cadr tb)
        ul (list (car ll) (cadr ur))
        lr (list (car ur) (cadr ll))
  )
  (command "._pline" ll lr ur ul "Close")
  (command "._ucs" "p")
 (princ)
)
```

The second routine, which follows, accomplishes the same task as the first routine by performing the geometric calculations with the AutoLISP functions `sin`  and `cos`. The result is correct only if the current UCS is parallel to the plane of the text object.

```lisp
(defun C:TBOX2 ( / textent ang sinrot cosrot t1 t2 p0 p1 p2 p3 p4)
  (setq textent (entget (car (entsel "\nSelect text: "))))
  (setq p0 (cdr (assoc 10 textent))
        ang (cdr (assoc 50 textent))
        sinrot (sin ang)
        cosrot (cos ang)
        t1 (car (textbox textent))
        t2 (cadr (textbox textent))
        p1 (list
               (+ (car p0)
                 (- (* (car t1) cosrot)(* (cadr t1) sinrot))
               )
               (+ (cadr p0)
                 (+ (* (car t1) sinrot)(* (cadr t1) cosrot))
               )
             )
        p2 (list
               (+ (car p0)
                 (- (* (car t2) cosrot)(* (cadr t1) sinrot))
               )
               (+ (cadr p0)
                 (+ (* (car t2) sinrot)(* (cadr t1) cosrot))
               )
             )
        p3 (list
               (+ (car p0)
                 (- (* (car t2) cosrot)(* (cadr t2) sinrot))
               )
               (+ (cadr p0)
                 (+ (* (car t2) sinrot)(* (cadr t2) cosrot))
               )
             )
        p4 (list
               (+ (car p0)
                 (- (* (car t1) cosrot)(* (cadr t2) sinrot))
               )
               (+ (cadr p0)
                 (+ (* (car t1) sinrot)(* (cadr t2) cosrot))
               )
             )
  )
  (command "._pline" p1 p2 p3 p4 "c")
 (princ)
)
```
