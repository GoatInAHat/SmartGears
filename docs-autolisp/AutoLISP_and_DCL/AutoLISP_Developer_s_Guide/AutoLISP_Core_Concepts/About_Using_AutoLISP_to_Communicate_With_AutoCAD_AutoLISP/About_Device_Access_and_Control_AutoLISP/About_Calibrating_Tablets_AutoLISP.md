---
title: About Calibrating Tablets (AutoLISP)
guid: "GUID-3790F2E6-439A-4FC3-A4BB-61116508B4D0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3790F2E6-439A-4FC3-A4BB-61116508B4D0.htm"
generated: "2025-11-28T19:06:10.284678Z"
description: Digitizing tablets can be calibrated using the TABLET command or with the AutoLISP tablet function.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# About Calibrating Tablets (AutoLISP)

> Digitizing tablets can be calibrated using the TABLET command or with the AutoLISP tablet function.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3790F2E6-439A-4FC3-A4BB-61116508B4D0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3790F2E6-439A-4FC3-A4BB-61116508B4D0.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Important:
 Tablet support is limited to Windows only.

The `tablet`  function enables applications to manage calibration settings directly and by saving those settings for future use. The first argument to the `tablet`  function is an integer *code*. If *code*  is equal to 0, the function returns the current calibration. If code is equal to 1, the calibration is set according to the remaining arguments. Calibrations are expressed as four 3D points (in addition to the *code*).

The first three points— *row1*, *row2*, and *row3* —are the three rows of the tablet's transformation matrix. The fourth point, *direction*, is a vector that is normal to the plane in which the tablet's surface is assumed to lie (expressed in WCS, the World Coordinate System). When the calibration is set with the TABLET command, the tablet's surface is assumed to lie in the *XY*  plane of the current UCS.

Note:
 The TABMODE system variable controls whether Tablet mode is turned on (1) or off (0). You can control it by using the
setvar
 function.

The following code retrieves the current tablet calibration and stores it in the variable `tcal`:

```lisp
(defun C:TABGET ()
  (setq tcal (tablet 0))
  (if tcal
    (princ
      (strcat "\nConfiguration saved, "
              "use TABSET to retrieve calibration."
      )
    )
    (princ "\nCalibration not obtainable ")
  )
 (princ)
)
```

If the TABGET command was successful, the `tcal`  variable now contains a list returned by the tablet function. This list might appear as follows:

```lisp
(1 (0.00561717 -0.000978942 -7.5171)
  (0.000978942 0.00561717 -9.17308)
  (0.0 0.0 1.0)
  (0.0 0.0 1.0)
)
```

To reset the calibration to the values retrieved by the preceding routine, you can use the following code:

```lisp
(defun C:TABSET ()
  (if (not (apply 'tablet tcal))
    (princ "\nUnable to reset calibration. ")
    (progn
      (princ "\nTablet calibration reset. ")
      (setvar "tabmode" 1)
      (if (= (getvar "tabmode") 0)
        (princ "\nUnable to turn on tablet mode ")
      )
    )
  )
 (princ)
)
```

## Defining the Transformation Matrix for a Tablet

Arguments *row1*, *row2*, and *row3*  are passed as a 3×3 transformation matrix which is meant to transform a 2D point. The 2D point is expressed as a column vector in homogeneous coordinates (by appending 1.0 as the third element), so the transformation looks like this:

The calculation of a point is similar to the 3D case. AutoCAD transforms the point by using the following formulas:

The resulting vector from the transformation can be turned back into a 2D point by dividing the first two (X',Y') components by the third component (the scale factor D') yielding. The resulting 2D point looks like (X'/D',Y'/D').

For projective transformations, the most general case, `tablet`  does the full calculation. But for affine and orthogonal transformations, M ~20~  and M ~21~  are both 0, so D' would be 1.0. The calculation of D' and the division are omitted; the resulting 2D point is simply (X',Y').

As the previous paragraph implies, an affine transformation is a special, uniform case of a projective transformation. An orthogonal transformation is a special case of an affine transformation: not only are M ~20~  and M ~21~  zero, but M ~00~  = M ~11~  and M ~10~  = -M ~01~.

Note:
 When you set a calibration, the list returned does not equal the list provided if the
direction
 is not normalized. AutoCAD normalizes the direction vector before it returns it. Also, it ensures the third element in the third column (
row3
[Z]
) is equal to 1. This situation should not arise if you set the calibration by using values retrieved from AutoCAD by means of
tablet
. However, it can happen if your program calculates the transformation itself.
