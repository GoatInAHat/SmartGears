---
title: getcfg (AutoLISP)
guid: "GUID-D8D46A8C-82DF-4193-A1B7-3DC3CAC70650"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D8D46A8C-82DF-4193-A1B7-3DC3CAC70650.htm"
generated: "2025-11-28T19:06:30.450109Z"
description: Obsolete. Retrieves application data from the AppData section of the acad20xx.cfg file
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

# getcfg (AutoLISP)

> Obsolete. Retrieves application data from the AppData section of the acad20xx.cfg file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D8D46A8C-82DF-4193-A1B7-3DC3CAC70650.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D8D46A8C-82DF-4193-A1B7-3DC3CAC70650.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

Note:
 This function might be removed in a future release. As an alternative, use the
vl-registry-read
 function to retrieve application data from the Windows Registry on Windows or property list file on Mac OS.

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(getcfg
cfgname
)
```

- ***cfgname*:** **Type:**  String  Section name and parameter value to retrieve (maximum length of 496 characters).

The *cfgname*  argument must be a string of the following form:

```lisp
"AppData/
application_name
/
section_name
/.../
param_name
"
```

## Return Values

**Type:**  String or nil

Application data, if successful. If *cfgname*  is not valid, `getcfg`  returns `nil`.

## Examples

Assuming the WallThk parameter in the AppData/ArchStuff section has a value of 8, the following command retrieves that value:

```lisp
(getcfg "AppData/ArchStuff/WallThk")

"8"
```
