---
title: ver (AutoLISP)
guid: "GUID-6E5DF4E1-5386-47A2-AEF3-488AA068FA15"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6E5DF4E1-5386-47A2-AEF3-488AA068FA15.htm"
generated: "2025-11-28T19:06:44.722907Z"
description: Returns a string that contains the current AutoLISP version number
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

# ver (AutoLISP)

> Returns a string that contains the current AutoLISP version number

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6E5DF4E1-5386-47A2-AEF3-488AA068FA15.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6E5DF4E1-5386-47A2-AEF3-488AA068FA15.htm)
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
(ver)
```

No arguments.

## Return Values

**Type:**  String

Textual value returned takes the following form:

```lisp
"
environment

version
 (
nn
)"
```

where *environment*  is the name of the AutoLISP environment, *version*  is the current version number, and *nn*  is a two-letter language description.

Examples of the two-letter language descriptions are as follows:

(de) German

(en) US/UK

(es) Spanish

(fr) French

(it) Italian

## Remarks

The `ver`  function can be used to check the compatibility of programs.

## Examples

- **Windows and Web:** **(ver)**  "Visual LISP <release> (en)"
- **Mac OS:** **(ver)**  "MacOS AutoLISP <release> (en)"
