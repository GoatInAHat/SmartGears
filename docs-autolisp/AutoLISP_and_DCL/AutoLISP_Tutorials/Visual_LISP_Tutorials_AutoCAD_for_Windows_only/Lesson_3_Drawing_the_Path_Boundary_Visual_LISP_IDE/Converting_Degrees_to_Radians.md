---
title: Converting Degrees to Radians
guid: "GUID-9DE7B38B-6FD6-44F9-9172-B0B7D27F60DF"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9DE7B38B-6FD6-44F9-9172-B0B7D27F60DF.htm"
generated: "2025-11-28T19:06:57.404604Z"
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

# Converting Degrees to Radians

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9DE7B38B-6FD6-44F9-9172-B0B7D27F60DF.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9DE7B38B-6FD6-44F9-9172-B0B7D27F60DF.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

You will now create a function to prevent you from repetitively typing an equation. It looks like this:

```lisp
(defun Degrees->Radians (numberOfDegrees)
(* pi (/ numberOfDegrees 180.0)))
```

This function is called `Degrees->Radians`. The function name indicates its purpose.

Why do you need a function to convert angular measurements? Behind the scenes, AutoCAD ®  uses radian angular measurement to keep track of angles, whereas most people think in terms of degrees. This function in your toolkit allows you to think in degrees, and lets AutoLISP ®  convert those numbers to radians.

## To test the utility function

1. Enter the following at the Visual LISP Console prompt:

   ```lisp
   (defun Degrees->Radians (numberOfDegrees)
   (* pi (/ numberOfDegrees 180.0)))
   ```
2. Enter the following at the Visual LISP Console prompt:

   ```lisp
   (degrees->radians 180)
   ```

   The function returns the number 3.14159. According to how this function works, 180 degrees is equivalent to 3.14159 radians.

To use this function within your program, simply copy the function definition from the Console window into your *gpmain.lsp*  file. You can paste it anywhere in the file, as long as you do not paste it into the middle of an existing function.

To clean up your work, select the text you just pasted in, then click the Format Selection button; Visual LISP will properly indent and format the code. ![](../../../../_assets/GUID_2FE8F167_09A5_48B9_B60C_7557554FD989-ff54f45c.png)

Next, add some comments describing the function. When you have fully documented the function, your code should look something like this:

```lisp
;;;--------------------------------------------------------------;
;;;     Function: Degrees->Radians                               ;
;;;--------------------------------------------------------------;
;;; Description: This function converts a number representing an ;
;;;              angular measurement in degrees, into its radian ;
;;;              equivalent. There is no error checking on the   ;
;;;              numberOfDegrees parameter -- it is always       ;
;;;              expected to be a valid number.                  ;
;;;--------------------------------------------------------------;
(defun Degrees->Radians (numberOfDegrees)
  (* pi (/ numberOfDegrees 180.0))
)
```
