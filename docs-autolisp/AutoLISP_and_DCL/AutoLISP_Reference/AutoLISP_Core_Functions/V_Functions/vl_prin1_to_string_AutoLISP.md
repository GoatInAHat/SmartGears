---
title: "vl-prin1-to-string (AutoLISP)"
guid: "GUID-4AA25012-28D5-4723-8AD3-B8BF9AE62425"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4AA25012-28D5-4723-8AD3-B8BF9AE62425.htm"
generated: "2025-11-28T19:06:48.828611Z"
description: Returns the string representation of LISP data as if it were output by the prin1 function
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

# vl-prin1-to-string (AutoLISP)

> Returns the string representation of LISP data as if it were output by the prin1 function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4AA25012-28D5-4723-8AD3-B8BF9AE62425.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4AA25012-28D5-4723-8AD3-B8BF9AE62425.htm)
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
(vl-prin1-to-string
data
)
```

- ***data*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  Any AutoLISP data.

## Return Values

**Type:**  String

A textual value containing the printed representation of *data*  as if displayed by `prin1`.

## Examples

- **Windows:** **(vl-prin1-to-string "abc")**  "\"abc\"" **(vl-prin1-to-string "c:\\acadwin")**  "\"C:\\\\ACADWIN\"" **(vl-prin1-to-string 'my-var)**  "MY-VAR"
- **Mac OS:** **(vl-prin1-to-string "abc")**  "\"abc\"" **(vl-prin1-to-string "/myutilities")**  "\"/myutilities\"" **(vl-prin1-to-string 'my-var)**  "MY-VAR"
