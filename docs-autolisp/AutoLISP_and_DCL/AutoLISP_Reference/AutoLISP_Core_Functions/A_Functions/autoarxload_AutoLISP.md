---
title: autoarxload (AutoLISP)
guid: "GUID-E1DDBABF-6F72-4747-B9E2-ACEEEA9FAA48"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E1DDBABF-6F72-4747-B9E2-ACEEEA9FAA48.htm"
generated: "2025-11-28T19:06:24.717779Z"
description: Predefines command names to load an associated ObjectARX file
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

# autoarxload (AutoLISP)

> Predefines command names to load an associated ObjectARX file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E1DDBABF-6F72-4747-B9E2-ACEEEA9FAA48.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E1DDBABF-6F72-4747-B9E2-ACEEEA9FAA48.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows and Mac OS only

## Signature

```lisp
(autoarxload
filename cmdlist
)
```

- ***filename*:** **Type:**  String  ObjectARX file to be loaded when one of the commands defined by the *cmdlist*  argument is entered at the Command prompt. You can omit the path from the *filename*, AutoCAD looks for the file in the support file search path. The extension from the filename can also be omitted; *.arx*  (Windows) or *.bundle*  (Mac OS).
- ***cmdlist*:** **Type:**  List  A list of strings.

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

The first time a user enters a command specified in *cmdlist*, AutoCAD loads the ObjectARX application specified in *filename*, then continues the command.

If you associate a command with *filename*  and that command is not defined in the specified file, AutoCAD alerts you with an error message when you enter the command.

Important:
 Starting with AutoCAD 2014-based products, custom applications must work under secure mode; when the SECURELOAD system variable is set to 1 or 2. When operating under secure mode, the program is restricted to loading and executing files that contain code from trusted locations; trusted locations are specified by the TRUSTEDPATHS system variable.

## Examples

The following code defines the `C:APP1`, `C:APP2`, and `C:APP3`  functions to load the *bonusapp.arx*  or *bonusapp.bundle*  file:

```lisp
(autoarxload "BONUSAPP" '("APP1" "APP2" "APP3"))
```
