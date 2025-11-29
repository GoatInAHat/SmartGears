---
title: Working With an Association List
guid: "GUID-0C9E2828-B528-476E-B31E-642F1A08F009"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0C9E2828-B528-476E-B31E-642F1A08F009.htm"
generated: "2025-11-28T19:06:58.588747Z"
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

# Working With an Association List

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0C9E2828-B528-476E-B31E-642F1A08F009.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-0C9E2828-B528-476E-B31E-642F1A08F009.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

You can pass the association list in the `gp_PathData`  variable to the `gp:drawOutline`  function by invoking the function as follows:

```lisp
(gp:drawOutline gp_PathData)
```

Simple enough, but you also need to figure out how to process the information stored in the association list. The Visual LISP Inspect feature can help you determine what to do.

## To use the Visual LISP Inspect feature to analyze your association list

1.  Load the code that is in the text editor window.
2. Enter the following expression at the Console prompt:

   ```lisp
   (setq BoundaryData (gp:getPointInput))
   ```

   Visual LISP will store the information you provide in a variable named `BoundaryData`.
3. Respond to the prompts for start point, endpoint, and half-width.
4. Select the
   BoundaryData
    variable name in the Console window by double-clicking it.
5. Click View
    Inspect from the Visual LISP menu.

   Visual LISP displays a window like the following:

   The Inspect window shows you each sublist within the `BoundaryData`  variable.
6. Enter the following at the Visual LISP Console prompt:

   ```lisp
   (assoc 50 BoundaryData)
   ```

   The `assoc`  function returns the entry in the association list that is identified by the specified key. In this example, the specified key is 50; this is associated with the angle of the garden path (see Putting Association Lists to Use for a list of the key-value pairs defined for this application).
7. Enter the following at the Visual LISP Console prompt:

   ```lisp
   (cdr (assoc 50 BoundaryData))
   ```

   The `cdr`  function returns the second element, and any remaining elements after that, from a list. In this example, `cdr`  retrieves the angle value, which is the second and last element in the entry returned by the `assoc`  function.

   By this point, you should have no trouble understanding the following code fragment:

   ```lisp
   (setq PathAngle  (cdr (assoc 50 BoundaryData))
           Width      (cdr (assoc 40 BoundaryData))
           HalfWidth  (/ Width 2.00)
           StartPt    (cdr (assoc 10 BoundaryData))
           PathLength (cdr (assoc 41 BoundaryData))
   ```
