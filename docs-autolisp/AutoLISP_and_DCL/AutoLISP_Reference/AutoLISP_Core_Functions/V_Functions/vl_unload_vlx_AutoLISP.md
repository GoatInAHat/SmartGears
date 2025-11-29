---
title: "vl-unload-vlx (AutoLISP)"
guid: "GUID-7AF6AC48-3435-4625-B210-C8FFFF1D2717"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7AF6AC48-3435-4625-B210-C8FFFF1D2717.htm"
generated: "2025-11-28T19:06:51.778071Z"
description: Unload a VLX application that is loaded in its own namespace
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

# vl-unload-vlx (AutoLISP)

> Unload a VLX application that is loaded in its own namespace

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7AF6AC48-3435-4625-B210-C8FFFF1D2717.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7AF6AC48-3435-4625-B210-C8FFFF1D2717.htm)
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
(vl-unload-vlx
appname
)
```

- ***appname*:** **Type:**  String  Name of a VLX application that is loaded in its own namespace. Do not include the *.vlx*  extension.

## Return Values

**Type:**  T or error

`T`  if successful; otherwise `vl-unload-vlx`  results in an error.

## Remarks

The `vl-unload-vlx`  function does not unload VLX applications that are loaded in the current document's namespace.

VLX files are supported on Windows only.

Note:
 This function is supported on Mac OS and Web, but does not affect the program.

## Examples

Assuming that *vlxns*  is an application that is loaded in its own namespace, the following command unloads *vlxns*:

```lisp
(vl-unload-vlx "vlxns")

T
```

Try unloading *vlxns*  again:

```lisp
(vl-unload-vlx "vlxns")

; *** ERROR: LISP Application is not found VLXNS
```

The `vl-unload-vlx`  command fails this time, because the application was not loaded.
