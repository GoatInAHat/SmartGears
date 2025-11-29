---
title: "vl-filename-extension (AutoLISP)"
guid: "GUID-6AF5AC95-2E38-4D2C-8A81-8D26F7E80375"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6AF5AC95-2E38-4D2C-8A81-8D26F7E80375.htm"
generated: "2025-11-28T19:06:47.695472Z"
description: Returns the extension from a file name, after stripping out the rest of the name
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

# vl-filename-extension (AutoLISP)

> Returns the extension from a file name, after stripping out the rest of the name

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6AF5AC95-2E38-4D2C-8A81-8D26F7E80375.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6AF5AC95-2E38-4D2C-8A81-8D26F7E80375.htm)
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
(vl-filename-extension
filename
)
```

- ***filename*:** **Type:**  String  File name, including the extension. The `vl-filename-extension`  function does not check to see if the specified file exists.

## Return Values

**Type:**  String or nil

A string containing the extension of *filename*. The returned string starts with a period (.) and is in uppercase. If *filename*  does not contain an extension, `vl-filename-extension`  returns `nil`.

## Examples

- **Windows:** **(vl-filename-extension "c:\\acadwin\\output.txt")**  ".txt" **(vl-filename-extension "c:\\acadwin\\output")**  nil
- **Mac OS and Web:** **(vl-filename-extension "/myutilities/support/output.txt")**  ".txt" **(vl-filename-extension "/myutilities/support/output")**  nil
