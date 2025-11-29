---
title: boole (AutoLISP)
guid: "GUID-C8412B41-BED9-4182-94E6-9EDB3C176D13"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C8412B41-BED9-4182-94E6-9EDB3C176D13.htm"
generated: "2025-11-28T19:06:24.951782Z"
description: Serves as a general bitwise Boolean function
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

# boole (AutoLISP)

> Serves as a general bitwise Boolean function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C8412B41-BED9-4182-94E6-9EDB3C176D13.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C8412B41-BED9-4182-94E6-9EDB3C176D13.htm)
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
(boole
operator int1 [int2 ...]
)
```

- ***operator*:** **Type:**  Integer  An integer between 0 and 15 representing one of the 16 possible Boolean functions in two variables.
- ***int1, int2...*:** **Type:**  Integer  A numeric value.  Note:  `boole`  will accept a single integer argument, but the result is unpredictable.

## Return Values

**Type:**  Integer

A numeric value.

## Remarks

Successive integer arguments are bitwise (logically) combined based on this function and on the following truth table:

| Boolean truth table |  |  |
| --- | --- | --- |
| Int1 | Int2 | operator bit |
| 0 | 0 | 8 |
| 0 | 1 | 4 |
| 1 | 0 | 2 |
| 1 | 1 | 1 |

Each bit of *int1*  is paired with the corresponding bit of *int2*, specifying one horizontal row of the truth table. The resulting bit is either 0 or 1, depending on the setting of the *operator*  bit that corresponds to this row of the truth table.

If the appropriate bit is set in *operator*, the resulting bit is 1; otherwise the resulting bit is 0. Some of the values for *operator*  are equivalent to the standard Boolean operations `AND`, `OR`, `XOR`, and `NOR`.

| Boole function bit values |  |  |
| --- | --- | --- |
| Operator | Operation | Resulting bit is 1 if |
| 1 | AND | Both input bits are 1 |
| 6 | XOR | Only one of the two input bits is 1 |
| 7 | OR | Either or both of the input bits are 1 |
| 8 | NOR | Both input bits are 0 (1's complement) |

## Examples

The following specifies a logical `AND`  of the values 12 and 5:

```lisp
(boole 1 12 5)

3
```

You can use other values of *operator*  to perform other Boolean operations for which there are no standard names. For example, if *operator*  is 4, the resulting bits are set if the corresponding bits are set in *int2*  but not in *int1*:

```lisp
(boole 4 3 14)

12
```
