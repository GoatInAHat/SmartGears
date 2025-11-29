---
title: About Object Snaps (AutoLISP)
guid: "GUID-4EEE5488-01D8-454F-9386-79E493E55D6E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4EEE5488-01D8-454F-9386-79E493E55D6E.htm"
generated: "2025-11-28T19:06:08.229192Z"
description: The osnap function can find a point by using one of the AutoCAD Object Snap modes.
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

# About Object Snaps (AutoLISP)

> The osnap function can find a point by using one of the AutoCAD Object Snap modes.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4EEE5488-01D8-454F-9386-79E493E55D6E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4EEE5488-01D8-454F-9386-79E493E55D6E.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

You pass the function a three element list that represents a 3D point; if you want to specify a 2D point, set the *Z*  axis to a value of 0 (zero). Snap modes are specified using a string value; multiple Snap modes can be specified by using a comma delimiter.

The following example code looks for the midpoint of an object near `pt1`:

```lisp
(setq pt2 (osnap pt1 "_midp"))
```

The following example code looks for the midpoint, the endpoint, or the center of an object nearest `pt1`:

```lisp
(setq pt2 (osnap pt1 "_midp,_endp,_center"))
```

Note:
 It is recommended to always add an underscore (_) in front of each Snap mode; this will help your program to work as expected when executed on an AutoCAD release other than the English language release.

In both examples, `pt2`  is set to the snap point if one is found that fulfills the osnap requirements. If more than one snap point fulfills the requirements, the point is selected based on the setting of the AutoCAD SORTENTS system variable. Otherwise, `pt2`  is set to `nil`.

Note:
 The AutoCAD APERTURE system variable determines the allowable proximity of a selected point to an object when you use Object Snap.
