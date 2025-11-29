---
title: tablet (AutoLISP)
guid: "GUID-5AD61389-1F8E-497B-8B0B-259488A50772"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5AD61389-1F8E-497B-8B0B-259488A50772.htm"
generated: "2025-11-28T19:06:43.347537Z"
description: Retrieves and sets digitizer (tablet) calibrations
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
  - function
---

# tablet (AutoLISP)

> Retrieves and sets digitizer (tablet) calibrations

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5AD61389-1F8E-497B-8B0B-259488A50772.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5AD61389-1F8E-497B-8B0B-259488A50772.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 02/12/2019

**Supported Platforms:**  Windows only

## Signature

```lisp
(tablet
code [row1 row2 row3 direction]
)
```

- ***code*:** **Type:**  Integer  One of the following:  **0**  -- Return the current digitizer calibration. In this case, the remaining arguments must be omitted.  **1**  -- Set the calibration according to the arguments that follow. In this case, you must provide the new calibration settings (*row1,* *row2,* *row3,*  and *direction*).
- ***row1, row2, row3*:** **Type:**  List  Three 3D points. These three arguments specify the three rows of the tablet's transformation matrix.  The third element in *row3*  (*Z*) should always equal 1: `tablet`  returns it as 1 even if you specify a different value in *row3*.
- ***direction*:** **Type:**  List  One 3D point. This is the vector (expressed in the world coordinate system, or WCS) that is normal to the plane that represents the surface of the tablet.  If the specified *direction*  isn't normalized, `tablet`  corrects it, so the *direction*  it returns when you set the calibration may differ from the value you passed.

## Return Values

**Type:**  Integer or nil

If `tablet`  fails, it returns `nil`  and sets the AutoCAD ERRNO system variable to a value that indicates the reason for the failure. This can happen if the digitizer is not a tablet.

## Examples

A very simple transformation that can be established with `tablet`  is the identity transformation:

```lisp
(tablet 1 '(1 0 0) '(0 1 0) '(0 0 1) '(0 0 1))
```

With this transformation in effect, AutoCAD will receive, effectively, raw digitizer coordinates from the tablet. For example, if you pick the point with digitizer coordinates (5000,15000), AutoCAD will see it as the point in your drawing with those same coordinates.

The AutoCAD TABMODE system variable allows AutoLISP routines to toggle the tablet on and off.
