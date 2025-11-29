---
title: About Unit Conversion (AutoLISP)
guid: "GUID-8256416C-3302-4BBF-B4C9-98A7973FBC96"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-8256416C-3302-4BBF-B4C9-98A7973FBC96.htm"
generated: "2025-11-28T19:06:09.197181Z"
description: "Values that represent distances, volumes, or other forms of measurement can be converted from one real-world unit to another."
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

# About Unit Conversion (AutoLISP)

> Values that represent distances, volumes, or other forms of measurement can be converted from one real-world unit to another.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-8256416C-3302-4BBF-B4C9-98A7973FBC96.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-8256416C-3302-4BBF-B4C9-98A7973FBC96.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The *acad.unt*  file defines various conversions between real-world units such as miles to kilometers, Fahrenheit to Celsius, and so on. The function `cvunit`  takes a value expressed in one system of units and returns the equivalent value in another system. The two systems of units are specified by strings containing expressions of units defined in *acad.unt*. The `cvunit`  function does not convert measurements of the same type. For example, it does not convert inches into grams.

The first time `cvunit`  converts to or from a unit during a drawing editor session, it must look up the string that specifies the unit in *acad.unt*. If your routine has many values to convert from one system of units to another, it is more efficient to convert the value 1.0 by a single call to `cvunit`  and then use the returned value as a scale factor in subsequent conversions. This works for all units defined in *acad.unt*  except temperature scales, which involve an offset as well as a scale factor.

The following example code converts a value in inches to centimeters:

```lisp
(cvunit 1.0 "inch" "cm")

2.54
```

The following example code converts a value from Fahrenheit to Celsius:

```lisp
(cvunit 32 "fahrenheit" "celsius")

3.46317e-009
```

The value returned after converting from Fahrenheit to Celsius is not exactly 0.0, you can use `rtos`  to control the precision of the value returned by `cvunit`. If you still need a real value, you can convert the string returned by `rtos`  with `atof`.

```lisp
(setq temp (cvunit 32 "fahrenheit" "celsius"))

3.46317e-009

(setq temp (rtos temp 2 2))

"0.00"

(setq temp (atof temp))

0.0
```
