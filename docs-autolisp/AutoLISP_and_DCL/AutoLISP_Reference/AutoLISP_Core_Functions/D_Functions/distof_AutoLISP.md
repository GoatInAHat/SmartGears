---
title: distof (AutoLISP)
guid: "GUID-CC0490B9-29BC-44C4-A317-5BAA34B0168E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CC0490B9-29BC-44C4-A317-5BAA34B0168E.htm"
generated: "2025-11-28T19:06:27.527218Z"
description: "Converts a string that represents a real (floating-point) value into a real value"
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

# distof (AutoLISP)

> Converts a string that represents a real (floating-point) value into a real value

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CC0490B9-29BC-44C4-A317-5BAA34B0168E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CC0490B9-29BC-44C4-A317-5BAA34B0168E.htm)
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
(distof
str [mode]
)
```

- ***str*:** **Type:**  String  A string to be converted. The argument must be a string that `distof`  can parse correctly according to the units specified by *mode*. It can be in the same form that `rtos`  returns, or in a form that AutoCAD allows for keyboard entry.
- ***mode*:** **Type:**  Integer  The units in which the string is currently formatted. The *mode*  corresponds to the values allowed for the AutoCAD LUNITS system variable. Specify one of the following numbers for *mode*:  **1**  -- Scientific  **2**  -- Decimal  **3**  -- Engineering (feet and decimal inches)  **4**  -- Architectural (feet and fractional inches)  **5**  -- Fractional

## Return Values

**Type:**  Real

A real number, if successful; otherwise `nil`.

Note:
 The
distof
 function treats modes 3 and 4 the same. That is, if
mode
 specifies 3 (engineering) or 4 (architectural) units, and
string
 is in either of these formats,
distof
 returns the correct real value.

## Remarks

The `distof`  and `rtos`  functions are complementary. If you pass `distof`  a string created by `rtos`, `distof`  is guaranteed to return a valid value, and vice versa (assuming the mode values are the same).

## Examples

None
