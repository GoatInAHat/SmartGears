---
title: Putting It All Together (AutoLISP)
guid: "GUID-8BE1EFA2-B861-4CF3-84DB-D0A825D84598"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8BE1EFA2-B861-4CF3-84DB-D0A825D84598.htm"
generated: "2025-11-28T19:06:59.327699Z"
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

# Putting It All Together (AutoLISP)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8BE1EFA2-B861-4CF3-84DB-D0A825D84598.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8BE1EFA2-B861-4CF3-84DB-D0A825D84598.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

You now have all the code you need to draw the outline of the garden path.

## To update your code

1.  Replace your old code for the
   gp:drawOutline
    function with the following:

   ```lisp
   ;;;---------------------------------------------------------------
   ;;;     Function: gp:drawOutline
   ;;;---------------------------------------------------------------
   ;;;  Description: This function will draw the outline of the garden
   ;;;               path.
   ;;;---------------------------------------------------------------
   ;;;  Note: No error checking or validation is performed on the
   ;;;  BoundaryData parameter.  The sequence of items within this
   ;;;  parameter does not matter, but it is assumed that all sublists
   ;;;  are present and contain valid data.
   ;;; --------------------------------------------------------------
   (defun gp:drawOutline (BoundaryData / VLADataPts PathAngle
                                         Width HalfWidth StartPt PathLength
                                         angm90 angp90 p1 p2
                                         p3 p4 polypoints pline
                         )
     ;; extract the values from the list BoundaryData
     (setq PathAngle (cdr (assoc 50 BoundaryData))
           Width (cdr (assoc 40 BoundaryData))
           HalfWidth (/ Width 2.00)
           StartPt (cdr (assoc 10 BoundaryData))
           PathLength (cdr (assoc 41 BoundaryData))
           angp90 (+ PathAngle (Degrees->Radians 90))
           angm90 (- PathAngle (Degrees->Radians 90))
           p1 (polar StartPt angm90 HalfWidth)
           p2 (polar p1 PathAngle PathLength)
           p3 (polar p2 angp90 Width)
           p4 (polar p3 (+ PathAngle (Degrees->Radians 180)) PathLength)
           polypoints (apply 'append
                        (mapcar '3dPoint->2dPoint (list p1 p2 p3 p4))
                      )
     )

     ;; ***** data conversion *****
     ;; Notice, polypoints is in AutoLISP format, consisting of a list
     ;; of the 4 corner points for the garden path.
     ;; The variable needs to be converted to a form of input parameter
     ;; acceptable to ActiveX calls.
     (setq VLADataPts (gp:list->variantArray polypoints))

     ;; Add polyline to the model space using ActiveX automation.
     (setq	pline (vla-addLightweightPolyline
                    *ModelSpace*; Global Definition for Model Space
                    VLADataPts
                 ) ;_ end of vla-addLightweightPolyline
     ) ;_ end of setq

     (vla-put-closed pline T)
     ;; Return the ActiveX object name for the outline polyline
     ;; The return value should look something like this:
     ;; #<VLA-OBJECT IAcadLWPolyline 02351a34>
     pline
   ) ;_ end of defun
   ```

   Note that `gp:drawOutline`  now returns the variable `pline`, not the quoted symbol `'SomeEname`  used in the stubbed-out version of the function.
2. Format the code you just entered by selecting it and choosing the Format Selection button on the Visual LISP toolbar.
3.  Enable ActiveX and add the global variable assignment for the pointer to model space, as described earlier. Scroll to the top of the text editor window and add the following code before the first
   defun
   :

   ```lisp
   ;;;--------------------------------------------------------------
   ;;; First step is to load ActiveX functionality. If ActiveX support
   ;;; already exists in document (can occur when Bonus tools have been
   ;;; loaded into AutoCAD), nothing happens. Otherwise, ActiveX
   ;;; support is loaded.
   ;;;---------------------------------------------------------------
   (vl-load-com)
   ;;; In Lesson 4, the following comment and code is moved to utils.lsp
   ;;;---------------------------------------------------------------
   ;;; For ActiveX functions, we need to define a global variable that
   ;;; "points" to the Model Space portion of the active drawing. This
   ;;; variable, named *ModelSpace* will be created at load time.
   ;;;---------------------------------------------------------------
   (setq *ModelSpace*
         (vla-get-ModelSpace
           (vla-get-ActiveDocument (vlax-get-acad-object))
         ) ;_ end of vla-get-ModelSpace
   ) ;_ end of setq
   ```

   Note how the above code lives outside of any `defun`. Because of this, Visual LISP automatically executes the code at the time you load the file.
4.  Look for the following line in the
   C:GPath
    function:

   ```lisp
   (setq PolylineName (gp:drawOutline))
   ```

   Change it to the following:

   ```lisp
   (setq PolylineName (gp:drawOutline gp_PathData))
   ```

   The `gp:drawOutline`  function is now expecting a parameter—the list containing the polyline boundary data—and this change fulfills that requirement.
5.  Add the
   gp:list->variantArray
    function shown in Constructing a Variant from a List of Points to the end of
   gpmain.lsp
   .

   Try loading and running the revised program. Visual LISP takes control away from AutoCAD before you see the end result, so switch back to the AutoCAD window after control returns to Visual LISP. If the program ran correctly, you should see a border for the garden path. If you find errors, debug the code and try again.
