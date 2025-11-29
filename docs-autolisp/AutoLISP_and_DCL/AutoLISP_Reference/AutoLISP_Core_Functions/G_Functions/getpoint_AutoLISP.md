---
title: getpoint (AutoLISP)
guid: "GUID-445F32F0-8A9D-4E1D-976F-DE87CC5267D0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-445F32F0-8A9D-4E1D-976F-DE87CC5267D0.htm"
generated: "2025-11-28T19:06:31.276918Z"
description: Pauses for user input of a point, and returns that point
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

# getpoint (AutoLISP)

> Pauses for user input of a point, and returns that point

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-445F32F0-8A9D-4E1D-976F-DE87CC5267D0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-445F32F0-8A9D-4E1D-976F-DE87CC5267D0.htm)
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
(getpoint
[pt] [msg]
)
```

- ***pt*:** **Type:**  List  A 2D or 3D base point in the current UCS.  Note:  `getpoint`  will accept a single integer or real number as the *pt*  argument, and use the AutoCAD direct distance entry mechanism to determine a point. This mechanism uses the value of the AutoCAD LASTPOINT system variable as the starting point, the *pt*  input as the distance, and the current cursor location as the direction from LASTPOINT. The result is a point that is the specified number of units away from LASTPOINT in the direction of the current cursor location.
- ***msg*:** **Type:**  String  Message to be displayed to prompt the user.

## Return Values

**Type:**  List or nil

A 3D point, expressed in terms of the current UCS.

## Remarks

The user can specify a point by pointing or by entering a coordinate in the current units format. If the *pt*  argument is present, AutoCAD draws a rubber-band line from that point to the current crosshairs position.

The user cannot enter another AutoLISP expression in response to a `getpoint`  request.

## Examples

```lisp
(setq p (getpoint))
(setq p (getpoint "Where? "))
(setq p (getpoint '(1.5 2.0) "Second point: "))
```
