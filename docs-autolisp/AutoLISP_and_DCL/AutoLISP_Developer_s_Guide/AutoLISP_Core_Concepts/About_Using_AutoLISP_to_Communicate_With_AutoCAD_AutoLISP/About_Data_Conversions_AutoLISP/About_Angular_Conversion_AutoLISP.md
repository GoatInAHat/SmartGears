---
title: About Angular Conversion (AutoLISP)
guid: "GUID-725BC3DE-3D4C-4365-A7F9-FAD5B8AC2DD1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-725BC3DE-3D4C-4365-A7F9-FAD5B8AC2DD1.htm"
generated: "2025-11-28T19:06:09.045338Z"
description: Angular values returned by most AutoLISP functions and those stored in a drawing are expressed in radians, while angular input is commonly provided in degrees or another angular format than radians.
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

# About Angular Conversion (AutoLISP)

> Angular values returned by most AutoLISP functions and those stored in a drawing are expressed in radians, while angular input is commonly provided in degrees or another angular format than radians.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-725BC3DE-3D4C-4365-A7F9-FAD5B8AC2DD1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-725BC3DE-3D4C-4365-A7F9-FAD5B8AC2DD1.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

You can convert angular values directly with a combination of math functions, or use the `angtos`  and `angtof`  functions. The `angtos`  function converts an angular value expressed in radians to degrees or one of the other supported angular formats. This function returns a string value. If you need a real (or floating point) value, you can use the `atof`  function to convert the string value that is returned by `angtos`.

```lisp
(setq half-PI (/ PI 2))

1.5708

(setq angstr (angtos half-PI 0 2))

"90.00"

(setq deg (atof angstr))

90.0
```

The `angtof`  function is the opposite of `angtos`, it converts a string representing an angular value into a real (or floating point) value in radians.

```lisp
(setq angstr (angtos 1.5708 1 6))

"90d0'0.76\""

(setq rad (angtof angstr 1))

1.5708
```

## Converting Radians to Degrees and Degrees to Radians with Math Functions

A more efficient method way to convert radians to degrees and degrees to radians than using the `angtos`  and `angtof`  functions is to use math functions.

The math formula to convert radians to degrees is:

```lisp
(Radians / PI) * 180 = Degrees
```

In AutoLISP, the same can be achieved using the following function:

```lisp
; Convert value in radians to degrees

(defun Radian->Degrees (nbrOfRadians)
  (* 180.0 (/ nbrOfRadians pi))
)

RADIAN->DEGREES

(Radian->Degrees PI)

180.0
```

The math formula to convert degrees to radians is:

```lisp
(Degrees / 180) * PI = Radians
```

In AutoLISP, the same can be achieved using the following function:

```lisp
; Convert value in degrees to radians

(defun Degrees->Radians (numberOfDegrees)
  (* pi (/ numberOfDegrees 180.0))
)

DEGREES->RADIANS

(Degrees->Radians 180.0)

3.14159
```
