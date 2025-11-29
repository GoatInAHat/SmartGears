---
title: "vl-vbaload (AutoLISP)"
guid: "GUID-54FD698E-8D94-4E27-9DF1-3F7E9D451E05"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-54FD698E-8D94-4E27-9DF1-3F7E9D451E05.htm"
generated: "2025-11-28T19:06:51.853033Z"
description: Loads a VBA project
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

# vl-vbaload (AutoLISP)

> Loads a VBA project

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-54FD698E-8D94-4E27-9DF1-3F7E9D451E05.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-54FD698E-8D94-4E27-9DF1-3F7E9D451E05.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows only; not available in AutoCAD LT for Windows, or on Mac OS and Web

## Signature

```lisp
(vl-vbaload
filename
)
```

- ***filename*:** **Type:**  String  Name of the VBA project file to be loaded.

## Return Values

**Type:**  String or error

A textual value containing the file name and path of the VBA project; otherwise an error occurs if the file is not found.

Important:
 Starting with AutoCAD 2014-based products, custom applications must work under secure mode; when the SECURELOAD system variable is set to 1 or 2. When operating under secure mode, the program is restricted to loading and executing files that contain code from trusted locations; trusted locations are specified by the TRUSTEDPATHS system variable.

## Release Information

- AutoCAD R14 and later on Windows

## History

- filename
   argument previously accepted an ASCII text string, but now accepts a Unicode text string.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(vl-vbaload "c:/program files/<AutoCAD installation directory>/sample/vba/drawline.dvb")

"c:\\program files\\<AutoCAD installation directory>\\sample\\vba\\drawline.dvb"
```
