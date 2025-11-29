---
title: lsh (AutoLISP)
guid: "GUID-3BC67533-3F4F-4C01-9053-CD82AA235AE8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3BC67533-3F4F-4C01-9053-CD82AA235AE8.htm"
generated: "2025-11-28T19:06:36.464328Z"
description: Returns the logical bitwise shift of an integer by a specified number of bits
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

# lsh (AutoLISP)

> Returns the logical bitwise shift of an integer by a specified number of bits

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3BC67533-3F4F-4C01-9053-CD82AA235AE8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3BC67533-3F4F-4C01-9053-CD82AA235AE8.htm)
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
(lsh
int numbits
)
```

- ***int*:** **Type:**  Integer  A numeric value.
- ***numbits*:** **Type:**  Integer  Number of bits to shift *int*.  If *numbits*  is positive, *int*  is shifted to the left; if *numbits*  is negative, *int*  is shifted to the right. In either case, zero bits are shifted in, and the bits shifted out are discarded.  If *numbits*  is not specified, no shift occurs.

## Return Values

**Type:**  Integer

The value of *int*  after the bitwise shift. The returned value is positive if the significant bit (bit number 31) contains a 0 after the shift operation; otherwise it is negative. If no arguments are supplied, `lsh`  returns 0.

The behavior is different from other languages (>> & << of C, C++, or Java) where more than 32 left shifts (of a 32 bit integer) result in 0. In right shift, the integer appears again on every 32 shifts.

## Examples

```lisp
(lsh 2 1)

4

(lsh 2 -1)

1

(lsh 40 2)

160
```
