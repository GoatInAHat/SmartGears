---
title: Converting 3D Points to 2D Points
guid: "GUID-D4E362BD-94E5-498B-B5F0-213B5FA8E89D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4E362BD-94E5-498B-B5F0-213B5FA8E89D.htm"
generated: "2025-11-28T19:06:57.505308Z"
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

# Converting 3D Points to 2D Points

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4E362BD-94E5-498B-B5F0-213B5FA8E89D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D4E362BD-94E5-498B-B5F0-213B5FA8E89D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Another useful function in the garden path program converts 3D points to 2D points. AutoCAD usually works with 3D coordinates, but some entities, such as lightweight polylines, are always meant to be 2D. The points returned by the `getpoint`  function are 3D, so you need to create a function to convert them.

## To convert a 3D point to a 2D point

1.  Enter the following at the Console window prompt:

   ```lisp
   (defun 3dPoint->2dPoint (3dpt)(list (car 3dpt) (cadr 3dpt)))
   ```
2. Test the function by entering the following at the Console prompt:

   ```lisp
   (3dpoint->2dpoint (list 10 20 0))
   ```

   This works, but there is another consideration for the garden path application. Although it often doesn't matter whether a number is an integer or a real in LISP functions, this isn't the case with ActiveX functions, which you'll use later in this lesson. ActiveX functions require real numbers. You can easily modify the function to ensure it returns reals instead of integers.
3. Enter the following code at the Console prompt:

   ```lisp
   (defun 3dPoint->2dPoint (3dpt)(list (float(car 3dpt))
   (float(cadr 3dpt))))
   ```
4. Run the function again:

   ```lisp
   (3dpoint->2dpoint (list 10 20 0))
   ```

   Notice the return values are now reals (indicated by the decimal values).
5. Test the function again, this time using the
   getpoint
    function. Enter the following at the Console prompt:

   ```lisp
   (setq myPoint(getpoint))
   ```
6. Pick a point in the AutoCAD drawing area.

   The `getpoint`  function returns a 3D point.
7. Enter the following at the Console prompt:

   ```lisp
   (3dPoint->2Dpoint myPoint)
   ```

   Note the 2D point returned.

   Now add the function to the *gpmain.lsp*  file, just as you did with `Degrees->Radians`. The new code should look like the following:

   ```lisp
   ;;;--------------------------------------------------------------;
   ;;; Function: 3dPoint->2dPoint                                   ;
   ;;;--------------------------------------------------------------;
   ;;; Description: This function takes one parameter representing a;
   ;;;              3D point (list of three integers or reals), and ;
   ;;;              converts it into a 2D point (list of two reals).;
   ;;;              There is no error checking on the 3D point      ;
   ;;;              parameter -- it is assumed to be a valid point. ;
   ;;;--------------------------------------------------------------;
   ;;; To do: Add some kind of parameter checking so that this      ;
   ;;;        function won't crash a program if it is passed a      ;
   ;;;        null value, or some other kind of data type than a    ;
   ;;;        3D point.                                             ;
   ;;;--------------------------------------------------------------;
   (defun 3dPoint->2dPoint (3dpt)
     (list (float(car 3dpt)) (float(cadr 3dpt)))
   )
   ```

   Note that the function heading includes a comment about some work you should do on this function in the future. If you want to earn some extra credit, think about how you would go about foolproofing this function so that invalid data does not make it crash.

   **Hint:**  `numberp`  and `listp`  functions…

   ```lisp
   (listp '(1 1 0)) => T
   (numberp 3.4) => T
   ```
