---
title: "vl-filename-directory (AutoLISP)"
guid: "GUID-AAB7ED2B-C119-4B30-A831-71A4D02757B0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AAB7ED2B-C119-4B30-A831-71A4D02757B0.htm"
generated: "2025-11-28T19:06:47.527317Z"
description: Returns the directory path of a file, after stripping out the name and extension
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

# vl-filename-directory (AutoLISP)

> Returns the directory path of a file, after stripping out the name and extension

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AAB7ED2B-C119-4B30-A831-71A4D02757B0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AAB7ED2B-C119-4B30-A831-71A4D02757B0.htm)
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
(vl-filename-directory
filename
)
```

- ***filename*:** **Type:**  String  Complete file name, including the path. The `vl-filename-directory`  function does not check to see if the specified file exists. Slashes (/) and backslashes (\) are accepted as directory delimiters.

## Return Values

**Type:**  String

A textual value containing the directory portion of *filename*, in uppercase.

## Examples

- **Windows:** **(vl-filename-directory "c:\\acadwin\\template.txt")**  "C:\\ACADWIN" **(vl-filename-directory "template.txt")**  ""
- **Mac OS and Web:** **(vl-filename-directory "/myutilities/support/template.txt")**  "/myutilities/support" **(vl-filename-directory "template.txt")**  ""
