---
title: "vl-vbarun (AutoLISP)"
guid: "GUID-75387617-9144-49CB-97E4-03B4CD29973B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-75387617-9144-49CB-97E4-03B4CD29973B.htm"
generated: "2025-11-28T19:06:51.930751Z"
description: Runs a VBA macro
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

# vl-vbarun (AutoLISP)

> Runs a VBA macro

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-75387617-9144-49CB-97E4-03B4CD29973B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-75387617-9144-49CB-97E4-03B4CD29973B.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows only; not available in AutoCAD LT for Windows, or on Mac OS and Web

## Signature

```lisp
(vl-vbarun
macroname
)
```

- ***macroname*:** **Type:**  String  Name of a macro in a loaded VBA project.

## Return Values

**Type:**  String or error

The value of the *macroname*  argument; otherwise, an error occurs.

## Release Information

- AutoCAD R14 and later on Windows

## Examples

Load a VBA project file:

```lisp
(vl-vbaload "c:/program files/<AutoCAD installation directory>/sample/vba/drawline.dvb")

"c:\\program files\\<AutoCAD installation directory>\\sample\\vba\\drawline.dvb"
```

Run a macro from the loaded project:

```lisp
(vl-vbarun "drawline")

"drawline"
```
