---
title: chr (AutoLISP)
guid: "GUID-5846BFDD-AE5F-4B2F-99E0-0B9DBA667739"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5846BFDD-AE5F-4B2F-99E0-0B9DBA667739.htm"
generated: "2025-11-28T19:06:25.464521Z"
description: "Converts an integer representing a character code into a single-character string"
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

# chr (AutoLISP)

> Converts an integer representing a character code into a single-character string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5846BFDD-AE5F-4B2F-99E0-0B9DBA667739.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5846BFDD-AE5F-4B2F-99E0-0B9DBA667739.htm)
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
(chr
int
)
```

- ***int*:** **Type:**  Integer  A numeric value in the range of 1-65536.

## Return Values

**Type:**  String

A single character based on the numeric value of *int*.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- int
   argument previously accepted an ASCII character code, but now accepts a Unicode character code.
- Return value changed from an integer in the range of 1-255 (ASCII character codes) to 1-65536 (Unicode character codes) and might be different than earlier releases. For example,
  (chr 128)
   previously returned "€", but now returns "". If you want to return "€", your code will need to be updated to
  (chr 8364)
  .
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  -  1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(chr 65)

"A"

(chr 66)

"B"

(chr 97)

"a"

(chr 8364)

"€"
```
