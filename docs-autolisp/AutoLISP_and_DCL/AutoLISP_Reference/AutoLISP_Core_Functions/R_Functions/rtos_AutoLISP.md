---
title: rtos (AutoLISP)
guid: "GUID-D03ABBC2-939A-44DB-8C93-FC63B64DE4A2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D03ABBC2-939A-44DB-8C93-FC63B64DE4A2.htm"
generated: "2025-11-28T19:06:40.153608Z"
description: Converts a number into a string
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

# rtos (AutoLISP)

> Converts a number into a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D03ABBC2-939A-44DB-8C93-FC63B64DE4A2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D03ABBC2-939A-44DB-8C93-FC63B64DE4A2.htm)
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
(rtos
number [mode [precision]]
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.
- ***mode*:** **Type:**  Integer  Linear units mode. The *mode*  corresponds to the values allowed for the AutoCAD LUNITS system variable.  The mode can be one of the following numbers:  **1**  -- Scientific  **2**  -- Decimal  **3**  -- Engineering (feet and decimal inches)  **4**  -- Architectural (feet and fractional inches)  **5**  -- Fractional
- ***precision*:** **Type:**  Integer  Precision used to format the returned value.

## Return Values

**Type:**  String

Formatted numeric value.

The AutoCAD UNITMODE system variable affects the returned string when engineering, architectural, or fractional units are selected (*mode*  values 3, 4, or 5).

## Remarks

The `rtos`  function returns a string that is the representation of *number*  according to the settings of *mode*, *precision*, and the AutoCAD UNITMODE, DIMZIN, LUNITS, and LUPREC system variables.

The *mode*  and *precision*  arguments correspond to the AutoCAD LUNITS and LUPREC system variables. If you omit the arguments, `rtos`  uses the current settings of LUNITS and LUPREC.

## Examples

Set variable `x`:

```lisp
(setq x 17.5)

17.5
```

Convert the value of `x`  to a string in scientific format, with a precision of 4:

```lisp
(setq fmtval (rtos x 1 4))

"1.7500E+01"
```

Convert the value of `x`  to a string in decimal format, with 2 decimal places:

```lisp
(setq fmtval (rtos x 2 2))

"17.50"
```

Convert the value of `x`  to a string in engineering format, with a precision of 2:

```lisp
(setq fmtval (rtos x 3 2))

"1'-5.50\""
```

Convert the value of `x`  to a string in architectural format:

```lisp
(setq fmtval (rtos x 4 2))

"1'-5 1/2\""
```

Convert the value of `x`  to a string in fractional format:

```lisp
(setq fmtval (rtos x 5 2))

"17 1/2"
```

Setting AutoCAD UNITMODE system variable to 1 causes units to be displayed as entered. This affects the values returned by `rtos`  for engineering, architectural, and fractional formats, as shown in the following examples:

```lisp
(setvar "unitmode" 1)

1

(setq fmtval (rtos x 3 2))

"1'5.50\""

(setq fmtval (rtos x 4 2))

"1'5-1/2\""

(setq fmtval (rtos x 5 2))

"17-1/2"
```
