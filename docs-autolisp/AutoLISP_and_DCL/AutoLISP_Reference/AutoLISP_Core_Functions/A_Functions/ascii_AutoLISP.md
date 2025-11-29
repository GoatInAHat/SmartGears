---
title: ascii (AutoLISP)
guid: "GUID-03E1B586-72CB-4AB6-A151-B581AE530318"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-03E1B586-72CB-4AB6-A151-B581AE530318.htm"
generated: "2025-11-28T19:06:23.540111Z"
description: Returns the conversion of the first character of a string into its character code (an integer)
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

# ascii (AutoLISP)

> Returns the conversion of the first character of a string into its character code (an integer)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-03E1B586-72CB-4AB6-A151-B581AE530318.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-03E1B586-72CB-4AB6-A151-B581AE530318.htm)
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
(ascii
str
)
```

- ***str*:** **Type:**  String  A single character or text string.

## Return Values

**Type:**  Integer

A numeric value in the range of 1-65536.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- str
   argument previously accepted an ASCII text string or character, but now accepts a Unicode text string or character.
- Return value was modified to support Unicode characters and might be different than earlier releases. For example,
  (ascii "€")
   previously returned 128, but now returns 8364.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  -  1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(ascii "A")

65

(ascii "a")

97

(ascii "BIG")

66

(ascii "€")

8364
```
