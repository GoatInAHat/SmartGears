---
title: cvunit (AutoLISP)
guid: "GUID-253E72F1-09B8-473E-A51B-B97BE2E29019"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-253E72F1-09B8-473E-A51B-B97BE2E29019.htm"
generated: "2025-11-28T19:06:26.205293Z"
description: Converts a value from one unit of measurement to another
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

# cvunit (AutoLISP)

> Converts a value from one unit of measurement to another

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-253E72F1-09B8-473E-A51B-B97BE2E29019.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-253E72F1-09B8-473E-A51B-B97BE2E29019.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows and Mac OS only

## Signature

```lisp
(cvunit
value from-unit to-unit
)
```

- ***value*:** **Type:**  Integer, Real, or List  The numeric value or point list (2D or 3D point) to be converted.
- ***from-unit*:** **Type:**  String  The unit that *value*  is being converted from.
- ***to-unit*:** **Type:**  String  The unit that *value*  is being converted to.

## Return Values

**Type:**  Real, List, or nil

The converted value, if successful; otherwise `nil`, if either unit name is unknown or not found in the *acad.unt*  file (or *acadlt.unt*  file in AutoCAD LT), or if the two units are incompatible (for example, trying to convert grams into years).

## Remarks

The *from-unit*  and *to-unit*  arguments can name any unit type found in the *acad.unt*  file (or *acadlt.unt*  file in AutoCAD LT).

## Examples

```lisp
(cvunit 1 "minute" "second")

60.0

(cvunit 1 "gallon" "furlong")

nil

(cvunit 1.0 "inch" "cm")

2.54

(cvunit 1.0 "acre" "sq yard")

4840.0

(cvunit '(1.0 2.5) "ft" "in")

(12.0 30.0)

(cvunit '(1 2 3) "ft" "in")

(12.0 24.0 36.0)
```

Note:
 If you have several values to convert in the same manner, it is more efficient to convert the value 1.0 once and then apply the resulting value as a scale factor in your own function or computation. This works for all predefined units except temperature, where an offset is involved as well.
