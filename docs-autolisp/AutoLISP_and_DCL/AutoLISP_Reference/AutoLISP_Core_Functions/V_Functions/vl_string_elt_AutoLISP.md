---
title: "vl-string-elt (AutoLISP)"
guid: "GUID-5A68B31E-1EA7-434A-93FF-C05D9514A55B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5A68B31E-1EA7-434A-93FF-C05D9514A55B.htm"
generated: "2025-11-28T19:06:50.190731Z"
description: Returns the character code for the character at a specified position in a string
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

# vl-string-elt (AutoLISP)

> Returns the character code for the character at a specified position in a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5A68B31E-1EA7-434A-93FF-C05D9514A55B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5A68B31E-1EA7-434A-93FF-C05D9514A55B.htm)
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
(vl-string-elt
str position
)
```

- ***str*:** **Type:**  String  A textual value to be inspected.
- ***position*:** **Type:**  Integer  A displacement in the string; the first character is displacement 0.

## Return Values

**Type:**  Integer or error

A numeric value denoting the character code for the character at the specified position.

Note:
 An error occurs if
position
 is outside the range of the
str
.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- str
   argument previously accepted an ASCII text string or character (range of 1-255), but now accepts a Unicode text string or character (range of 1-65536).
- Return value was modified to support Unicode characters and might be different than earlier releases. For example,
  (vl-string-elt "abc中" 3)
   previously returned 128, but now returns 20013.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(vl-string-elt "May the Force be with you" 8)

70
```
