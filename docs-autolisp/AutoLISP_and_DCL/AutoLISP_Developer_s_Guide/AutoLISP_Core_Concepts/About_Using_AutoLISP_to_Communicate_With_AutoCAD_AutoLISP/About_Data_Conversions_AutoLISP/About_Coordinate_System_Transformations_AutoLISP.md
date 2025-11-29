---
title: About Coordinate System Transformations (AutoLISP)
guid: "GUID-0F0B833D-78ED-4491-9918-9481793ED10B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0F0B833D-78ED-4491-9918-9481793ED10B.htm"
generated: "2025-11-28T19:06:09.524318Z"
description: A point or displacement can be transformed from one coordinate system into another with trans.
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

# About Coordinate System Transformations (AutoLISP)

> A point or displacement can be transformed from one coordinate system into another with trans .

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0F0B833D-78ED-4491-9918-9481793ED10B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0F0B833D-78ED-4491-9918-9481793ED10B.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The `trans`  function takes three arguments with an optional fourth. The first argument, *pt*, is either a 3D point or a 3D displacement vector, distinguished by an optional displacement argument called *disp*. The *disp*  argument must be nonzero if *pt*  is to be treated as a displacement vector; otherwise, *pt*  is treated as a point. The *from*  argument specifies the coordinate system in which *pt*  is expressed, and the *to*  argument specifies the desired coordinate system.

The following is the syntax for the `trans`  function:

```lisp
(trans
pt from to [disp]
)
```

Both the *from*  and *to*  arguments can specify a coordinate system in any of the following ways:

- As an integer code that specifies the WCS, current UCS, or current DCS (of either the current viewport or paper space).
- As an entity name returned by one of the entity name or selection set functions. This specifies the OCS of the named object. If the OCS does not differ, conversion between OCS and WCS is an identity operation.
- As a 3D extrusion vector. Extrusion vectors are always represented in World coordinates; an extrusion vector of (0,0,1) specifies the WCS itself.

The following table lists the valid integer codes that can be used as the *from*  and *to*  arguments:

| Coordinate system codes |  |
| --- | --- |
| Code | Coordinate system |
| 0 | World (WCS) |
| 1 | User (current UCS) |
| 2 | Display; DCS of current viewport when used with code 0 or 1, DCS of current model space viewport when used with code 3 |
| 3 | Paper space DCS, PSDCS (used only with code 2) |

The following example code translates a point from the WCS into the current UCS.

```lisp
(setq pt '(1.0 2.0 3.0))
(setq cs_from 0) ; WCS
(setq cs_to 1) ; UCS
(trans pt cs_from cs_to 0) ; disp = 0 indicates that pt is a point
```

If the current UCS is rotated 90 degrees counterclockwise around the World Z axis, the call to `trans`  returns a point (2.0,-1.0,3.0). However, if you swap the *to*  and *from*  values, the result differs as shown in the following code:

```lisp
(trans pt cs_to cs_from 0) ; the result is (-2.0,1.0,3.0)
```

## Coordinate Systems

- **WCS:** World coordinate system—the reference coordinate system. All other coordinate systems are defined relative to the WCS, which never changes. Values measured relative to the WCS are stable across changes to other coordinate systems.
- **UCS:** User coordinate system—the working coordinate system defined by the user to make drawing tasks easier. All points passed to AutoCAD commands, including those returned from AutoLISP routines and external functions, are points in the current UCS (unless the user precedes them with a **`*`**  at the AutoCAD Command prompt). If you want your application to send coordinates in the WCS, OCS, or DCS to AutoCAD commands, you must first convert them to the UCS by calling the trans function.
- **OCS:** Object coordinate system—point values returned by `entget`  are expressed in this coordinate system, relative to the object itself. These points are usually converted into the WCS, current UCS, or current DCS, according to the intended use of the object. Conversely, points must be translated into an OCS before they are written to the database by means of the `entmod`  or `entmake`  functions. This is also known as the entity coordinate system.
- **DCS:** Display coordinate system—the coordinate system into which objects are transformed before they are displayed. The origin of the DCS is the point stored in the AutoCAD system variable TARGET, and its *Z*  axis is the viewing direction. In other words, a viewport is always a plan view of its DCS. These coordinates can be used to determine where something will be displayed to the AutoCAD user.  When the *from*  and *to*  integer codes are 2 and 3, in either order, 2 indicates the DCS for the current model space viewport and 3 indicates the DCS for paper space (PSDCS). When the 2 code is used with an integer code other than 3 (or another means of specifying the coordinate system), it is assumed to indicate the DCS of the current space, whether paper space or model space. The other argument is also assumed to indicate a coordinate system in the current space.
- **PSDCS:** Paper space DCS—this coordinate system can be transformed only *to*  or *from*  the DCS of the currently active model space viewport. This is essentially a 2D transformation, where the *X*  and *Y*  coordinates are always scaled and are offset if the *disp*  argument is 0. The *Z*  coordinate is scaled but is never translated. Therefore, it can be used to find the scale factor between the two coordinate systems. The PSDCS (integer code 2) can be transformed only into the current model space viewport. If the from argument equals 3, the to argument must equal 2, and vice versa.
