---
title: angtos (AutoLISP)
guid: "GUID-C2D277B4-2F59-423F-9C56-E5A1DEFE0CCE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C2D277B4-2F59-423F-9C56-E5A1DEFE0CCE.htm"
generated: "2025-11-28T19:06:22.962015Z"
description: Converts an angular value in radians into a string
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

# angtos (AutoLISP)

> Converts an angular value in radians into a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C2D277B4-2F59-423F-9C56-E5A1DEFE0CCE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C2D277B4-2F59-423F-9C56-E5A1DEFE0CCE.htm)
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
(angtos
angle [unit [precision]]
)
```

- ***angle*:** **Type:**  Integer or Real  A number, in radians.
- ***unit*:** **Type:**  Integer  An integer that specifies the angular units. If *unit*  is omitted, `angtos`  uses the current value of the AutoCAD AUNITS system variable. The following *unit* s may be specified:  **0**  -- Degrees  **1**  -- Degrees/minutes/seconds  **2**  -- Grads  **3**  -- Radians  **4**  -- Surveyor's units
- ***precision*:** **Type:**  Integer  Specifies the number of decimal places of precision to be returned. If omitted, `angtos`  uses the current setting of the AutoCAD AUPREC system variable.

## Return Values

**Type:**  String or nil

A string, if successful; otherwise `nil`.

## Remarks

The `angtos`  function takes *angle*  and returns it edited into a string according to the settings of *unit*  and *precision*, the AutoCAD UNITMODE and DIMZIN system variables.

The `angtos`  function accepts a negative *angle*  argument, but always reduces it to a positive value between zero and 2 pi radians before performing the specified conversion.

The UNITMODE system variable affects the returned string when surveyor's units are selected (a *unit*  value of 4). If UNITMODE = 0, spaces are included in the string (for example, “N 45d E”); if UNITMODE = 1, no spaces are included in the string (for example, “N45dE”).

## Examples

```lisp
(angtos 0.785398 0 4)

"45.0000"

(angtos -0.785398 0 4)

"315.0000"

(angtos -0.785398 4)

"S 45d E"
```

Note:
 Routines that use the
angtos
 function to display arbitrary angles (those not relative to the value of AutoCAD ANGBASE system variable) should check and consider the value of ANGBASE.
