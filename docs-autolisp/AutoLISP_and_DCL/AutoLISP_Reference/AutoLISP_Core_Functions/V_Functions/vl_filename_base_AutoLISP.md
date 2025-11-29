---
title: "vl-filename-base (AutoLISP)"
guid: "GUID-DA20B039-C252-4751-AA2F-EFC314E7D5A0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DA20B039-C252-4751-AA2F-EFC314E7D5A0.htm"
generated: "2025-11-28T19:06:47.382072Z"
description: Returns the name of a file, after stripping out the directory path and extension
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

# vl-filename-base (AutoLISP)

> Returns the name of a file, after stripping out the directory path and extension

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DA20B039-C252-4751-AA2F-EFC314E7D5A0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DA20B039-C252-4751-AA2F-EFC314E7D5A0.htm)
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
(vl-filename-base
filename
)
```

- ***filename*:** **Type:**  String  File name. The `vl-filename-base`  function does not check to see if the file exists.

## Return Values

**Type:**  String

Textual value containing *filename*  in uppercase, with any directory and extension stripped from the name.

## Examples

- **Windows:** **(vl-filename-base "c:\\acadwin\\acad.exe")**  "ACAD" **(vl-filename-base "c:\\acadwin")**  "ACADWIN"
- **Mac OS and Web:** **(vl-filename-base "/myutilities/lsp/utilities.lsp")**  "utilities" **(vl-filename-base "/myutilities/support")**  "support"
