---
title: "vl-arx-import (AutoLISP)"
guid: "GUID-1729FD01-3773-45BD-8AFD-2CA3D4F1B111"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1729FD01-3773-45BD-8AFD-2CA3D4F1B111.htm"
generated: "2025-11-28T19:06:45.032797Z"
description: "Imports ObjectARX functions into a separate-namespace VLX"
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

# vl-arx-import (AutoLISP)

> Imports ObjectARX functions into a separate-namespace VLX

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1729FD01-3773-45BD-8AFD-2CA3D4F1B111.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1729FD01-3773-45BD-8AFD-2CA3D4F1B111.htm)
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
(vl-arx-import
['function | application]
)
```

- ***function*:** **Type:**  String  Symbol name for the function to import.
- ***application*:** **Type:**  String  Application name whose functions are to be imported.

## Return Values

**Type:**  nil

By default, separate-namespace VLX applications do not import any functions from ObjectARX applications. Use `vl-arx-import`  to explicitly import functions from ObjectARX applications.

If executed from a document VLX, this function does nothing and returns `nil`, as all ADS-DEFUN function names are automatically imported to document VLX applications.

## Remarks

If no argument (or `nil`) is specified, `vl-arx-import`  imports all function names from the current document namespace.

Note:
 While the function is supported on Mac OS and Web, VLX files are not supported on Mac OS and Web which results in different behavior than when used on Windows.

## Examples

To see how `vl-arx-import`  works, try the following:

1. Copy the following code into your AutoLISP editor and save the file:

   ```lisp
   (vl-doc-export 'testarx)
   (defun testarx ()
      (princ "This function tests an ObjectARX application ")
      (vl-arx-import 'c:cal)
      (c:cal)
   )
   ```
2. Use Make Application to build a VLX with this code. Select Separate-Namespace Application Options.
3. Load
   geomcal.arx
   , if it is not already loaded.
4. Load and run the application.

   To verify the effect of `vl-arx-import`, comment out the `vl-arx-import`  call in the code, save the change, then rebuild and run the application. Without the `vl-arx-import`  call, the `c:cal`  function will not be found.

In the example above, you could have replaced the `vl-arx-import`  call with the following:

```lisp
(vl-arx-import "geomcal.crx")
```

This would import all functions defined in *geomcal.crx*, including `c:cal`.
