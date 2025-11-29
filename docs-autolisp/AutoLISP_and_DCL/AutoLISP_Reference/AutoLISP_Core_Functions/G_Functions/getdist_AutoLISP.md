---
title: getdist (AutoLISP)
guid: "GUID-7D381B2A-79AA-4133-9C96-3DA63F8D8632"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7D381B2A-79AA-4133-9C96-3DA63F8D8632.htm"
generated: "2025-11-28T19:06:30.697560Z"
description: Pauses for user input of a distance
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

# getdist (AutoLISP)

> Pauses for user input of a distance

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7D381B2A-79AA-4133-9C96-3DA63F8D8632.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7D381B2A-79AA-4133-9C96-3DA63F8D8632.htm)
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
(getdist
[pt] [msg]
)
```

- ***pt*:** **Type:**  List  A 2D or 3D point to be used as the base point in the current UCS. If *pt*  is provided, the user is prompted for the second point.
- ***msg*:** **Type:**  String  Message to be displayed to prompt the user. If no string is supplied, AutoCAD does not display a message.

## Return Values

**Type:**  Real or nil

A numeric value. If a 3D point is provided, the returned value is a 3D distance. However, setting the 64-bit of the `initget`  function instructs `getdist`  to ignore the *Z*  component of 3D points and to return a 2D distance.

## Remarks

The user can specify the distance by selecting two points, or by specifying just the second point, if a base point is provided. The user can also specify a distance by entering a number in the AutoCAD current distance units format. Although the current distance units format might be in feet and inches (architectural), the `getdist`  function always returns the distance as a real.

The `getdist`  function draws a rubber-band line from the first point to the current crosshairs position to help the user visualize the distance.

The user cannot enter another AutoLISP expression in response to a `getdist`  request.

## Examples

```lisp
(setq dist (getdist))
(setq dist (getdist '(1.0 3.5)))
(setq dist (getdist "How far "))
(setq dist (getdist '(1.0 3.5) "How far? "))
```
