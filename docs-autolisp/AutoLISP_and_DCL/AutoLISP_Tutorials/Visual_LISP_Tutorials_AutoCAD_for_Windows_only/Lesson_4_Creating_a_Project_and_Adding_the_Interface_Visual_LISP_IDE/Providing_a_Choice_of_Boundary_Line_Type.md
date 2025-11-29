---
title: Providing a Choice of Boundary Line Type
guid: "GUID-D377C55C-466C-44D1-B049-6E714EEDA327"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D377C55C-466C-44D1-B049-6E714EEDA327.htm"
generated: "2025-11-28T19:07:01.611182Z"
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

# Providing a Choice of Boundary Line Type

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D377C55C-466C-44D1-B049-6E714EEDA327.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D377C55C-466C-44D1-B049-6E714EEDA327.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

One requirement specified for the garden path application was to allow users to draw the boundary outline as either a lightweight polyline or an old-style polyline. The first version of `gp:drawOutline`  you wrote always used a lightweight polyline to draw the boundary. Now that the dialog box interface is ready to go, you can build in the option for drawing an old-style polyline as well. To accomplish this, `gp:drawOutline`  must determine what kind of polyline to draw, and then it must draw it.

The necessary changes to `gp:drawOutline`  are included in the following code fragment. Make the modification from the *gpdraw.lsp*  file indicated in bold:

```lisp
(setq PathAngle  (cdr (assoc 50 BoundaryData))
    Width        (cdr (assoc 40 BoundaryData))
    HalfWidth    (/ Width 2.00)
    StartPt      (cdr (assoc 10 BoundaryData))
    PathLength   (cdr (assoc 41 BoundaryData))
    angp90       (+ PathAngle (Degrees->Radians 90))
    angm90       (- PathAngle (Degrees->Radians 90))
    p1           (polar StartPt angm90 HalfWidth)
    p2           (polar p1 PathAngle PathLength)
    p3           (polar p2 angp90 Width)
    p4           (polar p3 (+ PathAngle (Degrees->Radians 180)) PathLength)
    poly2Dpoints (apply 'append
                   (mapcar '3dPoint->2dPoint (list p1 p2 p3 p4))
                 )
    poly3Dpoints (mapcar 'float (append p1 p2 p3 p4))

    ;; get the polyline style
    plineStyle   (strcase (cdr (assoc 4 BoundaryData)))
) ;_ end of setq

;; Add polyline to the model space using ActiveX automation
(setq pline (if (= plineStyle "LIGHT")
              ;; create a lightweight polyline
              (vla-addLightweightPolyline
                      *ModelSpace*      ; Global Definition for Model Space
                      (gp:list->variantArray poly2Dpoints) ;data conversion
              ) ;_ end of vla-addLightweightPolyline

              ;; or create an old-style polyline
              (vla-addPolyline
                      *ModelSpace*
                      (gp:list->variantArray poly3Dpoints) ;data conversion
              ) ;_ end of vla-addPolyline
            ) ;_ end of if
) ;_ end of setq
```

Typing the changes into your code can be very tricky, as you not only need to add code but also to delete some existing lines and rearrange others. It is recommended you copy the entire `setq`  statement from the online tutorial and paste it into your code.
