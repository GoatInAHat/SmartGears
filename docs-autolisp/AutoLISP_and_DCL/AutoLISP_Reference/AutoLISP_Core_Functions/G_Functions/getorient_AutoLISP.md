---
title: getorient (AutoLISP)
guid: "GUID-4F8606DD-6453-4D01-84C6-EC8EEBBE2D1A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4F8606DD-6453-4D01-84C6-EC8EEBBE2D1A.htm"
generated: "2025-11-28T19:06:31.195953Z"
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

# getorient (AutoLISP)

> Pauses for user input of an angle, and returns that angle in radians

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4F8606DD-6453-4D01-84C6-EC8EEBBE2D1A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4F8606DD-6453-4D01-84C6-EC8EEBBE2D1A.htm)
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
(getorient
[pt] [msg]
)
```

- ***pt*:** **Type:**  List  A 2D base point in the current UCS.  The *pt*  argument, if specified, is assumed to be the first of two points, so that the user can show AutoLISP the angle by pointing to one other point. You can supply a 3D base point, but the angle is always measured in the current construction plane.
- ***msg*:** **Type:**  String  Message to be displayed to prompt the user.

## Return Values

**Type:**  Real or nil

The angle specified by the user, in radians, with respect to the current construction plane.

## Remarks

The `getorient`  function measures angles with the zero-radian direction to the right (east) and angles that are increasing in the counterclockwise direction. The angle input by the user is based on the current settings of the AutoCAD ANGDIR and ANGBASE system variables, but once an angle is provided, it is measured in a counterclockwise direction, with zero radians being to the right (ignoring ANGDIR and ANGBASE). Therefore, some conversion must take place if you select a different zero-degree base or a different direction for increasing angles by using the AutoCAD UNITS command or the ANGBASE and ANGDIR system variables.

Use `getangle`  when you need a rotation amount (a relative angle). Use `getorient`  to obtain an orientation (an absolute angle).

The user cannot enter another AutoLISP expression as the response to a `getorient`  request.

## Examples

```lisp
(setq pt1 (getpoint "Pick point: "))

(4.55028 5.84722 0.0)

(getorient pt1 "Pick point: ")

5.61582
```
