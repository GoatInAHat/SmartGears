---
title: atan (AutoLISP)
guid: "GUID-1A40A16E-8ABE-437D-9888-2F43CEA0B5CE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1A40A16E-8ABE-437D-9888-2F43CEA0B5CE.htm"
generated: "2025-11-28T19:06:24.045118Z"
description: Returns the arctangent of a number in radians
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

# atan (AutoLISP)

> Returns the arctangent of a number in radians

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1A40A16E-8ABE-437D-9888-2F43CEA0B5CE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1A40A16E-8ABE-437D-9888-2F43CEA0B5CE.htm)
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
(atan
num1 [num2]
)
```

- ***num1*:** **Type:**  Integer or Real  A numeric value.
- ***num2*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Real

The arctangent of *num1*, in radians, if only *num1*  is supplied. If you supply both *num1*  and *num2*  arguments, `atan`  returns the arctangent of *num1* /*num2*, in radians. If *num2*  is zero, it returns an angle of plus or minus 1.570796 radians (+90 degrees or -90 degrees), depending on the sign of *num1*. The range of angles returned is -pi/2 to +pi/2 radians.

## Examples

```lisp
(atan 1)

0.785398

(atan 1.0)

0.785398

(atan 0.5)

0.463648

(atan -1.0)

-0.785398

(atan 2.0 3.0)

0.588003

(atan 2.0 -3.0)

2.55359

(atan 1.0 0.0)

1.5708
```
