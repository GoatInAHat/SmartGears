---
title: angtof (AutoLISP)
guid: "GUID-9DE9200B-F27E-4BB6-8AE6-BC5C8A64A527"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9DE9200B-F27E-4BB6-8AE6-BC5C8A64A527.htm"
generated: "2025-11-28T19:06:22.883732Z"
description: "Converts a string representing an angle into a real (floating-point) value in radians"
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

# angtof (AutoLISP)

> Converts a string representing an angle into a real (floating-point) value in radians

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9DE9200B-F27E-4BB6-8AE6-BC5C8A64A527.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9DE9200B-F27E-4BB6-8AE6-BC5C8A64A527.htm)
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
(angtof
string [units]
)
```

- ***string*:** **Type:**  String  Formatted angular string that `angtof`  can parse correctly to the specified *unit*. It can be in the same form that `angtos`  returns, or in a form that AutoCAD allows for keyboard entry.
- ***units*:** **Type:**  Integer  Specifies the units in which the string is formatted. The value should correspond to values allowed for the AutoCAD AUNITS system variable. If *unit*  is omitted, `angtof`  uses the current value of AUNITS. The following *unit* s may be specified:  **0**  -- Degrees  **1**  -- Degrees/minutes/seconds  **2**  -- Grads  **3**  -- Radians  **4**  -- Surveyor's units

## Return Values

**Type:**  Real

A real value, if successful; otherwise `nil`.

The `angtof`  and `angtos`  functions are complementary: if you pass `angtof`  a string created by `angtos`, `angtof`  is guaranteed to return a valid value, and vice versa (assuming the *unit*  values match).

## Examples

```lisp
(angtof "45.0000")

0.785398

(angtof "45.0000" 3)

1.0177
```
