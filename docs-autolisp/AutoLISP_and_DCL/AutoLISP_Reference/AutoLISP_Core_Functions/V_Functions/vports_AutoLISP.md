---
title: vports (AutoLISP)
guid: "GUID-929FC8B2-1235-4B2A-B330-FF2E219E072C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-929FC8B2-1235-4B2A-B330-FF2E219E072C.htm"
generated: "2025-11-28T19:06:52.158370Z"
description: Returns a list of viewport descriptors for the current viewport configuration
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

# vports (AutoLISP)

> Returns a list of viewport descriptors for the current viewport configuration

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-929FC8B2-1235-4B2A-B330-FF2E219E072C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-929FC8B2-1235-4B2A-B330-FF2E219E072C.htm)
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
(vports)
```

No arguments.

## Return Values

**Type:**  List

One or more viewport descriptor lists consisting of the viewport identification number and the coordinates of the viewport's lower-left and upper-right corners.

If the AutoCAD TILEMODE system variable is set to 1 (on), the returned list describes the viewport configuration created with the AutoCAD VPORTS command. The corners of the viewports are expressed in values between 0.0 and 1.0, with (0.0, 0.0) representing the lower-left corner of the display screen's graphics area, and (1.0, 1.0) the upper-right corner. If TILEMODE is 0 (off), the returned list describes the viewport objects created with the AutoCAD MVIEW command. The viewport object corners are expressed in paper space coordinates. Viewport number 1 is always paper space when TILEMODE is off.

## Examples

Given a single-viewport configuration with TILEMODE on, the `vports`  function might return the following:

```lisp
((1 (0.0 0.0) (1.0 1.0)))
```

Given four equal-sized viewports located in the four corners of the screen when TILEMODE is on, the `vports`  function might return the following lists:

```lisp
((5 (0.5 0.0) (1.0 0.5))
 (2 (0.5 0.5) (1.0 1.0))
 (3 (0.0 0.5) (0.5 1.0))
 (4 (0.0 0.0) (0.5 0.5)))
```

The current viewport's descriptor is always first in the list. In the previous example, viewport number 5 is the current viewport.
