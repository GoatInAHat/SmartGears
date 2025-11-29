---
title: acad_helpdlg (AutoLISP)
guid: "GUID-23B68C07-57B2-4875-868D-E69C139CE22B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-23B68C07-57B2-4875-868D-E69C139CE22B.htm"
generated: "2025-11-28T19:06:21.289763Z"
description: Invokes the help facility (obsolete)
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

# acad_helpdlg (AutoLISP)

> Invokes the help facility (obsolete)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-23B68C07-57B2-4875-868D-E69C139CE22B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-23B68C07-57B2-4875-868D-E69C139CE22B.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows only; not available in AutoCAD LT for Windows

## Signature

```lisp
(acad_helpdlg
helpfile topic
)
```

- ***helpfile*:** **Type:**  String  The path and file name of the help file to open.
- ***topic*:** **Type:**  String  The name of the topic to display in the help file.

## Return Values

**Type:**  String or nil

Returns the file name of the help file if it is found; otherwise, `nil`  is returned if the file is not found.

## Remarks

This externally defined function has been replaced by the built-in `help`  function. It is provided for compatibility with earlier releases of AutoCAD.
