---
title: grvecs (AutoLISP)
guid: "GUID-44B8C277-4140-4F4C-ACF2-3332CC463021"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44B8C277-4140-4F4C-ACF2-3332CC463021.htm"
generated: "2025-11-28T19:06:32.435707Z"
description: Draws multiple vectors in the drawing area
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

# grvecs (AutoLISP)

> Draws multiple vectors in the drawing area

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44B8C277-4140-4F4C-ACF2-3332CC463021.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44B8C277-4140-4F4C-ACF2-3332CC463021.htm)
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
(grvecs
vlist [trans]
)
```

- ***vlist*:** **Type:**  List  A vector list is composed of a series of optional color integers and two point lists. See below for details on how to format *vlist*.
- ***trans*:** **Type:**  List  A transformation matrix used to change the location or proportion of the vectors defined in your vector list. This matrix is a list of four lists of four real numbers.

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

The format for *vlist*  is as follows:

```lisp
([color1] from1 to1 [color2] from2 to2 ...)
```

The color value applies to all succeeding vectors until *vlist*  specifies another color. AutoCAD colors are in the range 0-255. If the color value is greater than 255, succeeding vectors are drawn in *XOR ink*, which complements anything it draws over and which erases itself when overdrawn. If the color value is less than zero, the vector is highlighted. Highlighting depends on the display device. Most display devices indicate highlighting by a dashed line, but some indicate it by using a distinctive color.

A pair of point lists, *from*  and *to*, specify the endpoints of the vectors, expressed in the current UCS. These can be 2D or 3D points. You must pass these points as pairs—two successive point lists—or the `grvecs`  call will fail.

AutoCAD clips the vectors as required to fit on the screen.

## Examples

The following code draws five vertical lines in the drawing area, each a different color:

```lisp
(grvecs '(1 (1 2)(1 5)
Draws a red line from (
1,2
) to (
1,5
)

          2 (2 2)(2 5)
Draws a yellow line from (
2,2
) to (
2,5
)

          3 (3 2)(3 5)
Draws a green line from (
3,2
) to (
3,5
)

          4 (4 2)(4 5)
Draws a cyan line from (
4,2
) to (
4,5
)

          5 (5 2)(5 5)
Draws a blue line from (
5,2
) to (
5,5
)

) )
```

The following matrix represents a uniform scale of 1.0 and a translation of 5.0,5.0,0.0. If this matrix is applied to the preceding list of vectors, they will be offset by 5.0,5.0,0.0.

```lisp
'((1.0 0.0 0.0 5.0)
   (0.0 1.0 0.0 5.0)
   (0.0 0.0 1.0 0.0)
   (0.0 0.0 0.0 1.0)
)
```
