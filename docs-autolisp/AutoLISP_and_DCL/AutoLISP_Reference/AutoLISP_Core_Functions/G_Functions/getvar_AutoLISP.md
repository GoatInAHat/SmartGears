---
title: getvar (AutoLISP)
guid: "GUID-9C56BEA8-D473-4305-9E17-BEF7630C334D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C56BEA8-D473-4305-9E17-BEF7630C334D.htm"
generated: "2025-11-28T19:06:31.736047Z"
description: Retrieves the value of an AutoCAD system variable
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

# getvar (AutoLISP)

> Retrieves the value of an AutoCAD system variable

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C56BEA8-D473-4305-9E17-BEF7630C334D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C56BEA8-D473-4305-9E17-BEF7630C334D.htm)
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
(getvar
varname
)
```

- ***varname*:** **Type:**  String  Names of a system variable.  See the product's Help system for a list of current AutoCAD system variables.

## Return Values

**Type:**  Integer, Real, String, List, or nil

The value of the system variable; otherwise `nil`, if *varname*  is not a valid system variable.

## Examples

Get the current value of the fillet radius:

```lisp
(getvar 'FILLETRAD)

0.25
```
