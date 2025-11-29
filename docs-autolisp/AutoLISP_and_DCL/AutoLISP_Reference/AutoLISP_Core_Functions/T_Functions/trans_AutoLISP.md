---
title: trans (AutoLISP)
guid: "GUID-1A316343-0B68-4DBE-8F49-B4D601CB8FCC"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1A316343-0B68-4DBE-8F49-B4D601CB8FCC.htm"
generated: "2025-11-28T19:06:44.377662Z"
description: Translates a point (or a displacement) from one coordinate system to another
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

# trans (AutoLISP)

> Translates a point (or a displacement) from one coordinate system to another

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1A316343-0B68-4DBE-8F49-B4D601CB8FCC.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1A316343-0B68-4DBE-8F49-B4D601CB8FCC.htm)
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
(trans
pt from to [disp]
)
```

- ***pt*:** **Type:**  List  A list of three reals that can be interpreted as either a 3D point or a 3D displacement (vector).
- ***from*:** **Type:**  Integer, List, or Ename (entity name)  An integer code, entity name, or 3D extrusion vector identifying the coordinate system in which *pt*  is expressed. The integer code can be one of the following:  **0**  -- World (WCS)  **1**  -- User (current UCS)  **2**  -- If used with code 0 or 1, this indicates the Display Coordinate System (DCS) of the current viewport. When used with code 3, it indicates the DCS of the current model space viewport.  **3**  -- Paper space DCS (used *only*  with code 2)
- ***to*:** **Type:**  Integer, List, or Ename (entity name)  An integer code, entity name, or 3D extrusion vector identifying the coordinate system of the returned point. See the *from*  argument for a list of valid integer codes.
- ***disp*:** **Type:** List  If present and is not `nil`, this argument specifies that *pt*  is to be treated as a 3D displacement rather than as a point.

## Return Values

**Type:**  List

A 3D point (or displacement) in the requested *to*  coordinate system.

## Remarks

If you use an entity name for the *from*  or *to*  argument, it must be passed in the format returned by the `entnext`, `entlast`, `entsel`, `nentsel`, and `ssname`  functions. This format lets you translate a point to and from the Object Coordinate System (OCS) of a particular object. (For some objects, the OCS is equivalent to the WCS; for these objects, conversion between OCS and WCS is a null operation.) A 3D extrusion vector (a list of three reals) is another method of converting to and from an object's OCS. However, this does not work for those objects whose OCS is equivalent to the WCS.

The `trans`  function can also transform 2D points. It does this by setting the *Z*  coordinate to an appropriate value. The *Z*  component used depends on the *from*  coordinate system that was specified and on whether the value is to be converted as a point or as a displacement. If the value is to be converted as a displacement, the *Z*  value is always 0.0; if the value is to be converted as a point, the filled-in *Z*  value is determined as shown in the following table:

| Converted 2D point *Z*  values |  |
| --- | --- |
| From | Filled-in *Z*  value |
| WCS | 0.0 |
| UCS | Current elevation |
| OCS | 0.0 |
| DCS | Projected to the current construction plane (UCS *XY*  plane + current elevation) |
| PSDCS | Projected to the current construction plane (UCS *XY*  plane + current elevation) |

## Examples

In the following examples, the UCS is rotated 90 degrees counterclockwise around the WCS *Z*  axis:

```lisp
(trans '(1.0 2.0 3.0) 0 1)

(2.0 -1.0 3.0)

(trans '(1.0 2.0 3.0) 1 0)

(-2.0 1.0 3.0)
```

For example, to draw a line from the insertion point of a piece of text (without using Osnap), you convert the text object's insertion point from the text object's OCS to the UCS.

```lisp
(trans
text-insert-point text-ename 1
)
```

You can then pass the result to the From Point prompt.

Conversely, you must convert point (or displacement) values to their destination OCS before feeding them to `entmod`. For example, if you want to move a circle (without using the AutoCAD MOVE command) by the UCS-relative offset (1,2,3), you need to convert the displacement from the UCS to the circle's OCS:

```lisp
(trans '(1 2 3) 1 circle-ename)
```

Then you add the resulting displacement to the circle's center point.

For example, if you have a point entered by the user and want to find out which end of a line it looks closer to, you convert the user's point from the UCS to the DCS.

```lisp
(trans user-point 1 2)
```

Then you convert each of the line's endpoints from the OCS to the DCS.

```lisp
(trans endpoint line-ename 2)
```

From there you can compute the distance between the user's point and each endpoint of the line (ignoring the *Z*  coordinates) to determine which end looks closer.
