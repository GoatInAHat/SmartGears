---
title: "Understanding the ActiveX Code in Gp:drawOutline"
guid: "GUID-B19B0C74-960B-4AE4-AF4F-7F8B13EEC80C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-B19B0C74-960B-4AE4-AF4F-7F8B13EEC80C.htm"
generated: "2025-11-28T19:06:58.864913Z"
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

# Understanding the ActiveX Code in Gp:drawOutline

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-B19B0C74-960B-4AE4-AF4F-7F8B13EEC80C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-B19B0C74-960B-4AE4-AF4F-7F8B13EEC80C.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The `gp:drawOutline`  function issues ActiveX calls to display the path's polyline border in AutoCAD. The following code fragment uses ActiveX to draw the border:

```lisp
;; Add polyline to the model space using ActiveX automation.
(setq pline (vla-addLightweightPolyline
              *ModelSpace*	; Global Definition for Model Space
              VLADataPts	; vertices of path boundary
            ) ;_ end of vla-addLightweightPolyline
) ;_ end of setq
(vla-put-closed pline T)
```

The names of all AutoLISP ActiveX functions that work on AutoCAD objects are prefixed with `vla-`. For example, addLightweightPolyline is the name of an ActiveX method, and `vla-addLightweightPolyline`  is the AutoLISP function that invokes this method. The `vla-put-closed`  call updates the closed property of the `pline`  object, the polyline drawn by `vla-addLightweightPolyline`.

The Automation objects that factor into AutoLISP ActiveX calls abide by a few standard rules:

-  The first argument to a
  vla-put
  ,
  vla-get
  , or
  vla-
   method call is the object being modified or queried, for example,
  *ModelSpace*
   in the first function call and
  pline
   in the second call.
-  The return value of a
  vla-
   method call is a
  VLA-object
  , which can be used in subsequent calls. For example,
  vla-addLightweightPolyline
   yields a return object,
  pline
  , that is altered in the next ActiveX call.
-  The ActiveX object model is structured hierarchically. Objects are traversed from the application object at the topmost level down to individual drawing primitives, such as polyline and circle objects. Thus, the
  gp:drawOutline
   function is not yet complete, because the
  *ModelSpace*
   automation object must first be accessed through the root application object.
