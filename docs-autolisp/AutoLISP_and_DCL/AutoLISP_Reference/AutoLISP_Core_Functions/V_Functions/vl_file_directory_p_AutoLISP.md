---
title: "vl-file-directory-p (AutoLISP)"
guid: "GUID-7EEA5563-33A7-453C-9E96-860F7565808B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7EEA5563-33A7-453C-9E96-860F7565808B.htm"
generated: "2025-11-28T19:06:46.897710Z"
description: Determines if a file name refers to a directory
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

# vl-file-directory-p (AutoLISP)

> Determines if a file name refers to a directory

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7EEA5563-33A7-453C-9E96-860F7565808B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7EEA5563-33A7-453C-9E96-860F7565808B.htm)
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
(vl-file-directory-p
filename
)
```

- ***filename*:** **Type:**  String  File name. If you do not specify a full path name, `vl-file-directory-p`  searches only the AutoCAD default drawing directory.

## Return Values

**Type:**  T or nil

`T`, if *filename*  is the name of a directory; `nil`  if it is not.

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

- **Windows:** **(vl-file-directory-p "sample")**  T **(vl-file-directory-p "yinyang")**  nil **(vl-file-directory-p "c:/My Documents")**  T **(vl-file-directory-p "c:/My Documents/lisp/yinyang.lsp")**  nil
- **Mac OS and Web:** **(vl-file-directory-p "support")**  T **(vl-file-directory-p "xyz")**  nil **(vl-file-directory-p "/documents")**  T **(vl-file-directory-p "/documents/output.txt")**  nil
