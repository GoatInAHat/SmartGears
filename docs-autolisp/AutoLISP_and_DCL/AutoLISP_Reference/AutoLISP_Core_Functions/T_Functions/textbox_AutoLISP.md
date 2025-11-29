---
title: textbox (AutoLISP)
guid: "GUID-D6C31C79-ECBF-4C7D-9285-1CFA95AC0918"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D6C31C79-ECBF-4C7D-9285-1CFA95AC0918.htm"
generated: "2025-11-28T19:06:43.886069Z"
description: Measures a specified text object, and returns the diagonal coordinates of a box that encloses the text
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

# textbox (AutoLISP)

> Measures a specified text object, and returns the diagonal coordinates of a box that encloses the text

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D6C31C79-ECBF-4C7D-9285-1CFA95AC0918.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D6C31C79-ECBF-4C7D-9285-1CFA95AC0918.htm)
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
(textbox
elist
)
```

- ***elist*:** **Type:**  List  An entity definition list defining a text object, in the format returned by `entget`.  If fields that define text parameters other than the text itself are omitted from *elist*, the current (or default) settings are used.  The minimum list accepted by `textbox`  is that of the text itself.

## Return Values

**Type:**  List or nil

A list of two points, if successful; otherwise `nil`.

The points returned by `textbox`  describe the bounding box of the text object as if its insertion point is located at (0,0,0) and its rotation angle is 0. The first list returned is generally the point (0.0 0.0 0.0) unless the text object is oblique or vertical, or it contains letters with descenders (such as *g*  and *p*). The value of the first point list specifies the offset from the text insertion point to the lower-left corner of the smallest rectangle enclosing the text. The second point list specifies the upper-right corner of that box. Regardless of the orientation of the text being measured, the point list returned always describes the lower-left and upper-right corners of this bounding box.

## Examples

The following command supplies the text and accepts the current defaults for the remaining parameters:

```lisp
(textbox '((1 . "Hello world.")))

((0.000124126 -0.00823364 0.0) (3.03623 0.310345 0.0))
```
