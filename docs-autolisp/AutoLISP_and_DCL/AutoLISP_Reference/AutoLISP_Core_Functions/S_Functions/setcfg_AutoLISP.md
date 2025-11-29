---
title: setcfg (AutoLISP)
guid: "GUID-8BABF3B0-C8E1-49C9-A367-9823211C1F6B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8BABF3B0-C8E1-49C9-A367-9823211C1F6B.htm"
generated: "2025-11-28T19:06:40.354660Z"
description: Obsolete. Writes application data to the AppData section of the acad20xx.cfg file
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

# setcfg (AutoLISP)

> Obsolete. Writes application data to the AppData section of the acad20xx.cfg file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8BABF3B0-C8E1-49C9-A367-9823211C1F6B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8BABF3B0-C8E1-49C9-A367-9823211C1F6B.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

Note:
 This function might be removed in a future release. As an alternative, use the
vl-registry-write
 function to store application data in the Windows Registry on Windows or property list file on Mac OS.

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(setcfg
cfgname cfgval
)
```

- ***cfgname*:** **Type:**  String  Section and parameter to set with the value of *cfgval*. The *cfgname*  argument must be a string of the following form:  AppData/*application_name* /*section_name* /.../*param_name*  The string can be up to 496 characters long.
- ***cfgval*:** **Type:**  String  Application data. The string can be up to 512 characters in length. Larger strings are accepted by `setcfg`, but cannot be returned by `getcfg`.

## Return Values

If successful, `setcfg`  returns *cfgval*. If *cfgname*  is not valid, `setcfg`  returns `nil`.

## Examples

The following code sets the WallThk parameter in the AppData/ArchStuff section to 8, and returns the string “8”:

```lisp
(setcfg "AppData/ArchStuff/WallThk" "8")

"8"
```
