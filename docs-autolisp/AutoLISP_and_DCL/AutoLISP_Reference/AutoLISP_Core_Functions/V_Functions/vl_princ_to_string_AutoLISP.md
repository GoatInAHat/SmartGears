---
title: "vl-princ-to-string (AutoLISP)"
guid: "GUID-33B0CA43-84F2-46DE-88BE-8FFF22C83FAD"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33B0CA43-84F2-46DE-88BE-8FFF22C83FAD.htm"
generated: "2025-11-28T19:06:48.915348Z"
description: Returns the string representation of LISP data as if it were output by the princ function
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

# vl-princ-to-string (AutoLISP)

> Returns the string representation of LISP data as if it were output by the princ function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33B0CA43-84F2-46DE-88BE-8FFF22C83FAD.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-33B0CA43-84F2-46DE-88BE-8FFF22C83FAD.htm)
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
(vl-princ-to-string
data
)
```

- ***data*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  Any AutoLISP data.

## Return Values

**Type:**  String

A string containing the printed representation of *data*  as if displayed by `princ`.

## Examples

- **Windows:** **(vl-princ-to-string "abc")**  "abc" **(vl-princ-to-string "c:\\acadwin")**  "C:\\ACADWIN" **(vl-princ-to-string 'my-var)**  "MY-VAR"
- **Mac OS:** **(vl-princ-to-string "abc")**  "abc" **(vl-princ-to-string "/myutilities")**  "/myutilities" **(vl-princ-to-string 'my-var)**  "MY-VAR"
