---
title: "vl-doc-import (AutoLISP)"
guid: "GUID-68A6C7D0-8EE9-4F78-829A-86D08D2BD193"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-68A6C7D0-8EE9-4F78-829A-86D08D2BD193.htm"
generated: "2025-11-28T19:06:46.179078Z"
description: Imports a previously exported function into a VLX namespace
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

# vl-doc-import (AutoLISP)

> Imports a previously exported function into a VLX namespace

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-68A6C7D0-8EE9-4F78-829A-86D08D2BD193.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-68A6C7D0-8EE9-4F78-829A-86D08D2BD193.htm)
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
(vl-doc-import
application ['function ...]
)
```

- ***application*:** **Type:**  String  Name of the VLX application whose functions are to be imported. Do not include the *.vlx*  extension in the name.
- ***function*:** **Type:**  Subroutine  One or more symbols naming functions to be imported. If no functions are specified, all functions exported by *application*  will be imported.

## Return Values

**Type:**  nil or error

`nil`  if successful; otherwise an error occurs

## Remarks

This function can be used in a separate-namespace VLX to import a function that was previously exported from another VLX loaded from the same document.

The `vl-doc-import`  function should be used only at the top level in a file, and never inside other forms (for example, not within a `defun`).

Note:
 While the function is supported on Mac OS and Web, VLX files are not supported on Mac OS and Web which results in different behavior than when used on Windows.

## Examples

Import function `ldataget`  from the `ldatatest`  application:

```lisp
(vl-doc-import "ldatatest" 'ldataget)

nil
```
