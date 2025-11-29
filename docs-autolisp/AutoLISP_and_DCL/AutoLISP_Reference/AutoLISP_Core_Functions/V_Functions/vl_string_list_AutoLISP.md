---
title: "vl-string->list (AutoLISP)"
guid: "GUID-3C1B15F7-CF97-4783-A725-231C3A1ABB11"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3C1B15F7-CF97-4783-A725-231C3A1ABB11.htm"
generated: "2025-11-28T19:06:50.106085Z"
description: Converts a string into a list of character codes
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

# vl-string->list (AutoLISP)

> Converts a string into a list of character codes

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3C1B15F7-CF97-4783-A725-231C3A1ABB11.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3C1B15F7-CF97-4783-A725-231C3A1ABB11.htm)
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
(vl-string->list
str
)
```

- ***str*:** **Type:**  String  A textual value.

## Return Values

**Type:**  List or nil

A list, each element of which is an integer representing the character codes of the corresponding characters in *str*.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- str
   argument previously accepted an ASCII text string or character, but now accepts a Unicode text string or character.
- Return value was modified to support Unicode characters and might be different than earlier releases. For example,
  (vl-string->list "1€")
   previously returned (49 128), but now returns (49 8364).
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(vl-string->list "")

nil

(vl-string->list "12")

(49 50)
```
