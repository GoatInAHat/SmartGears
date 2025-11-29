---
title: "vl-file-size (AutoLISP)"
guid: "GUID-646032BF-CAC1-4166-9EBA-2C954DFF3005"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-646032BF-CAC1-4166-9EBA-2C954DFF3005.htm"
generated: "2025-11-28T19:06:47.111192Z"
description: Determines the size of a file, in bytes
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

# vl-file-size (AutoLISP)

> Determines the size of a file, in bytes

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-646032BF-CAC1-4166-9EBA-2C954DFF3005.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-646032BF-CAC1-4166-9EBA-2C954DFF3005.htm)
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
(vl-file-size
filename
)
```

- ***filename*:** **Type:**  String  Naming the file to be sized. If you do not specify a full path name, `vl-file-size`  searches the AutoCAD default drawing directory for the file.

## Return Values

**Type:**  Integer

If successful, `vl-file-size`  returns an integer showing the size of *filename*. If the file is not readable, `vl-file-size`  returns `nil`. If *filename*  is a directory or an empty file, `vl-file-size`  returns 0.

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

- **Windows:** **(vl-file-size "c:/autoexec.bat")**  1417 **(vl-file-size "c:/")**  0  In the preceding example, `vl-file-size`  returned 0 because *c:/*  names a directory.
- **Mac OS and Web:** **(vl-file-size "/output.txt")**  568 **(vl-file-size "/")**  0  In the preceding example, `vl-file-size`  returned 0 because */*  names the <root> of the drive and not a file.
