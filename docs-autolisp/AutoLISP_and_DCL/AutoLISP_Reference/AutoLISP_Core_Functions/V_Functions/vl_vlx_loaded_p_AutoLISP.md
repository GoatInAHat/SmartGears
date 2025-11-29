---
title: "vl-vlx-loaded-p (AutoLISP)"
guid: "GUID-2322688B-F1FA-416E-AEF0-E14256717588"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2322688B-F1FA-416E-AEF0-E14256717588.htm"
generated: "2025-11-28T19:06:52.074900Z"
description: "Determines whether a separate-namespace VLX is currently loaded"
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

# vl-vlx-loaded-p (AutoLISP)

> Determines whether a separate-namespace VLX is currently loaded

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2322688B-F1FA-416E-AEF0-E14256717588.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2322688B-F1FA-416E-AEF0-E14256717588.htm)
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
(vl-vlx-loaded-p
appname
)
```

- ***appname*:** **Type:**  String  Name of a VLX application.

## Return Values

**Type:**  T or nil

`T`  if the application is loaded, `nil`  if it is not loaded.

## Remarks

VLX files are supported on Windows only.

Note:
 This function is supported on Mac OS and Web, but does not affect the program.

## Examples

Check to see if the *vlxns*  application is loaded in its own namespace:

```lisp
(vl-vlx-loaded-p "vlxns")

nil
```

The application is not loaded in its own namespace.

Now load *vlxns*:

```lisp
(load "vlxns.vlx")

nil
```

Check to see if the *vlxns*  application loaded successfully:

```lisp
(vl-vlx-loaded-p "vlxns")

T
```

This example assumes *vlxns*  was defined to run in its own namespace. If the application was not defined to run in its own namespace, it would load into the current document's namespace and `vl-vlx-loaded-p`  would return `nil`.
