---
title: "vl-file-systime (AutoLISP)"
guid: "GUID-7F6A31B2-7D51-4C4D-B239-206F398D67CA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7F6A31B2-7D51-4C4D-B239-206F398D67CA.htm"
generated: "2025-11-28T19:06:47.190305Z"
description: Returns last modification time of the specified file
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

# vl-file-systime (AutoLISP)

> Returns last modification time of the specified file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7F6A31B2-7D51-4C4D-B239-206F398D67CA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7F6A31B2-7D51-4C4D-B239-206F398D67CA.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows and Mac OS only

## Signature

```lisp
(vl-file-systime
filename
)
```

- ***filename*:** **Type:**  String  Name of the file to be checked.

## Return Values

**Type:**  List or nil

A list containing the modification date and time; otherwise `nil`, if the file is not found.

The list returned contains the following elements:

- year
- month
- day of week
- day of month
- hours
- minutes
- seconds

Note:
 Monday is day 1 of day of week, Tuesday is day 2, and so on.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- filename
   argument previously accepted an ASCII text string, but now accepts a Unicode text string.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

- **Windows:** **(vl-file-systime**  **"c:/program files/<AutoCAD installation directory>/sample/visuallisp/yinyang.lsp")**  (1998 4 3 8 10 6 52)  The returned value shows that the file was last modified in 1998, in the 4th month of the year (April), the 3rd day of the week (Wednesday), on the 8th day of the month, at 10:6:52.
- **Mac OS:** **(vl-file-systime "/output.txt")**  (2011 5 4 26 16 3 51 586)  The returned value shows that the file was last modified in 2011, in the 5th month of the year (May), the 4th day of the week (Thursday), on the 26th day of the month, at 4:03:51 PM.
