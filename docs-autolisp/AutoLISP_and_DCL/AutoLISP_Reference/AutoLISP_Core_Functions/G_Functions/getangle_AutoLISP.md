---
title: getangle (AutoLISP)
guid: "GUID-947F34FA-5E58-4C7C-A169-556D4B8E2208"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-947F34FA-5E58-4C7C-A169-556D4B8E2208.htm"
generated: "2025-11-28T19:06:30.369252Z"
description: Pauses for user input of an angle, and returns that angle in radians
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
  - function
---

# getangle (AutoLISP)

> Pauses for user input of an angle, and returns that angle in radians

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-947F34FA-5E58-4C7C-A169-556D4B8E2208.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-947F34FA-5E58-4C7C-A169-556D4B8E2208.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(getangle
[pt] [msg]
)
```

- ***pt*:** **Type:**  List  A 2D base point in the current UCS.  The *pt*  argument, if specified, is assumed to be the first of two points, so the user can show AutoLISP the angle by pointing to one other point. You can supply a 3D base point, but the angle is always measured in the current construction plane.
- ***msg*:** **Type:**  String  Message to be displayed to prompt the user.

## Return values

**Type:**  Real or nil

The angle specified by the user, in radians.

The `getangle`  function measures angles with the zero-radian direction (set by the AutoCAD ANGBASE system variable) with angles increasing in the counterclockwise direction. The returned angle is expressed in radians with respect to the current construction plane (the *XY*  plane of the current UCS, at the current elevation).

## Remarks

Users can specify an angle by entering a number in the AutoCAD current angle units format. Although the current angle units format might be in degrees, grads, or some other unit, this function always returns the angle in radians. The user can also show AutoLISP the angle by pointing to two 2D locations in the drawing area. AutoCAD draws a rubber-band line from the first point to the current crosshairs position to help you visualize the angle.

It is important to understand the difference between the input angle and the angle returned by `getangle`. Angles that are passed to `getangle`  are based on the current settings of the AutoCAD ANGDIR and ANGBASE system variables. However, once an angle is provided, it is measured in a counterclockwise direction (ignoring ANGDIR) with zero radians as the current setting of ANGBASE.

The user cannot enter another AutoLISP expression as the response to a `getangle`  request.

## Examples

The following code examples show how different arguments can be used with `getangle`:

```lisp
(setq ang (getangle))
(setq ang (getangle '(1.0 3.5)))
(setq ang (getangle "Which way? "))
(setq ang (getangle '(1.0 3.5) "Which way? "))
```
