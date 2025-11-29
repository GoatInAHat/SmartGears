---
title: "acet-ps-to-ms (AutoLISP)"
guid: "GUID-0B38CD6E-1A28-4029-B233-81DB17619F66"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B38CD6E-1A28-4029-B233-81DB17619F66.htm"
generated: "2025-11-28T19:06:22.357102Z"
description: Converts a real value from paper space units to model space units
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 13/01/2021
topic_subtype:
  - autolisp
  - function
---

# acet-ps-to-ms (AutoLISP)

> Converts a real value from paper space units to model space units

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B38CD6E-1A28-4029-B233-81DB17619F66.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B38CD6E-1A28-4029-B233-81DB17619F66.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 13/01/2021

**Supported Platforms:**  Windows only

**Prerequisites:**  The AcSpaceTrans ObjectARX application must be loaded before the function can be called, `(arxload "acspacetrans.arx")`  or `(arxload "acspacetrans.crx")`  based on release.

## Signature

```lisp
(acet-ps-to-ms
[value] [viewport]
)
```

- ***value*:** **Type:**  Real  Value to be converted.
- ***viewport*:** **Type:**  Ename (entity name)  A viewport entity name.

## Return Values

**Type:**  Real or nil

The converted real value on success; `nil`  on failure.

## Remarks

If both the *value*  and *viewport*  arguments are specified, the value is converted to model space units using the specified viewport. No user input is required.

If only the *value*  argument is specified, the current viewport is assumed and no user input is required. However, if the current space is model space, there is no current viewport and the function will fail (returning `nil`). If paper space is the current space, the function will either prompt for a viewport if more than one viewport exists in the layout, or use the single existing viewport.

If no arguments are specified, the function prompts for a value and converts it if possible.

## Examples

None
