---
title: getcorner (AutoLISP)
guid: "GUID-21BE8290-7F11-400B-AC39-62A110F07545"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-21BE8290-7F11-400B-AC39-62A110F07545.htm"
generated: "2025-11-28T19:06:30.624706Z"
description: Pauses for user input of a rectangle's second corner
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

# getcorner (AutoLISP)

> Pauses for user input of a rectangle's second corner

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-21BE8290-7F11-400B-AC39-62A110F07545.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-21BE8290-7F11-400B-AC39-62A110F07545.htm)
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
(getcorner
pt [msg]
)
```

- ***pt*:** **Type:**  List  A point to be used as the base point.
- ***msg*:** **Type:**  String  Message to be displayed to prompt the user.

## Return Values

**Type:**  List or nil

The `getcorner`  function returns a point in the current UCS, similar to `getpoint`. If the user supplies a 3D point, its *Z*  coordinate is ignored. The current elevation is used as the *Z*  coordinate.

## Remarks

The `getcorner`  function takes a base point argument, based on the current UCS, and draws a rectangle from that point as the user moves the crosshairs on the screen.

The user cannot enter another AutoLISP expression in response to a `getcorner`  request.

## Examples

```lisp
(getcorner '(7.64935 6.02964 0.0))

(17.2066 1.47628 0.0)

(getcorner '(7.64935 6.02964 0.0) "\nPick a corner")

Pick a corner(15.9584 2.40119 0.0)
```
