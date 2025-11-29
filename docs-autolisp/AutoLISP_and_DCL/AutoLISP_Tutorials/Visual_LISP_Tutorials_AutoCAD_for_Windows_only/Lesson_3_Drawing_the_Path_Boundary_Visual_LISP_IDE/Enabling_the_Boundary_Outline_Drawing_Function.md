---
title: Enabling the Boundary Outline Drawing Function
guid: "GUID-1A0CA306-C203-40C8-83AB-E75CEBEE7770"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-1A0CA306-C203-40C8-83AB-E75CEBEE7770.htm"
generated: "2025-11-28T19:06:58.089962Z"
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

# Enabling the Boundary Outline Drawing Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-1A0CA306-C203-40C8-83AB-E75CEBEE7770.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-1A0CA306-C203-40C8-83AB-E75CEBEE7770.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

After the last lesson, the `gp:drawOutline`  function looked like the following:

```lisp
;;;--------------------------------------------------------------;
;;;     Function: gp:drawOutline                                 ;
;;;--------------------------------------------------------------;
;;; Description: This function draws the outline of the          ;
;;;              garden path.                                    ;
;;;--------------------------------------------------------------;
(defun gp:drawOutline ()
  (alert
    (strcat "This function will draw the outline of the polyline "
      "\nand return a polyline entity name/pointer."
    )
  )
  ;; For now, simply return a quoted symbol.  Eventually, this
  ;; function will return an entity name or pointer.
  'SomeEname
)
```

As it exists, the code does not do much. However, using the association list information stored in the variable `gp_PathData`, you have enough information to calculate the points for the path boundary. You now have to determine how to pass the information in that variable to `gp:drawOutline`.

Remember `gp_PathData`  is a local variable defined within the `C:GPath`  function. In AutoLISP, local variables declared in one function are visible to any function called from that function (refer to Differentiating between Local and Global Variables for clarification). The `gp:drawOutline`  function is called from within `C:GPath`. You can refer to the `gp_PathData`  variable in `gp:drawOutline`, but this is not a good programming practice.

Why? When the two functions using the same variable are defined in the same file, as in the examples shown so far, it is not too difficult to figure out where the variable is defined and what it is used for. But if the functions are defined in different files—as is often the case—you would have to search through both files to figure out what `gp_PathData`  represents.
