---
title: "vl-mkdir (AutoLISP)"
guid: "GUID-02D5CC9E-9394-4630-AE14-133F5F03D7B1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-02D5CC9E-9394-4630-AE14-133F5F03D7B1.htm"
generated: "2025-11-28T19:06:48.561530Z"
description: Creates a directory
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

# vl-mkdir (AutoLISP)

> Creates a directory

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-02D5CC9E-9394-4630-AE14-133F5F03D7B1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-02D5CC9E-9394-4630-AE14-133F5F03D7B1.htm)
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
(vl-mkdir
directoryname
)
```

- ***directoryname*:** **Type:**  String  The name of the directory you want to create.

## Return Values

**Type:**  T or nil

`T`  if successful, `nil`  if the directory exists or if unsuccessful.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- directoryname
   argument previously accepted an ASCII text string, but now accepts a Unicode text string.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

- **Windows:** **(vl-mkdir "c:\\mydirectory")**  T
- **Mac OS and Web:** **(vl-mkdir "/mydirectory")**  T
