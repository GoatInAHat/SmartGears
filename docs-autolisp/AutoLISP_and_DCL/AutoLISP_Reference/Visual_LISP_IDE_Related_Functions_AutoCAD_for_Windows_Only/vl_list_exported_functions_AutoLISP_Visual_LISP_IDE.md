---
title: "vl-list-exported-functions (AutoLISP/Visual LISP IDE)"
guid: "GUID-3D87AA2E-DB4B-4B28-B5A8-04A8183BF8AD"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3D87AA2E-DB4B-4B28-B5A8-04A8183BF8AD.htm"
generated: "2025-11-28T19:06:54.254956Z"
description: Lists exported functions
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

# vl-list-exported-functions (AutoLISP/Visual LISP IDE)

> Lists exported functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3D87AA2E-DB4B-4B28-B5A8-04A8183BF8AD.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3D87AA2E-DB4B-4B28-B5A8-04A8183BF8AD.htm)
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
(vl-list-exported-functions
[appname]
)
```

- ***appname*:** **Type:**  String  Name of a loaded VLX application. Do *not*  include the *.vlx*  extension.

## Return Values

**Type:**  List

A list of strings naming exported functions; otherwise `nil`, if there are no functions exported from the specified VLX. If *appname*  is omitted or is `nil`, `vl-list-exported-functions`  returns a list of all exported functions (for example, *c:*  functions) except those exported from VLX namespaces.

## Remarks

VLX files are supported on Windows only.

Note:
 This function is supported on Mac OS and Web, but does not affect the program.

## Examples

```lisp
(vl-list-exported-functions "whichexpns")

("WHICHNAMESPACE")
```
