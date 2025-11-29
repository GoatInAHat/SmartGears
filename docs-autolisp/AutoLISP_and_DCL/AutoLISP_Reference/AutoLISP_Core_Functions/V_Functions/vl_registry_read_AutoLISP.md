---
title: "vl-registry-read (AutoLISP)"
guid: "GUID-0B9E26CF-2410-482E-B568-7FE05B9F013C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B9E26CF-2410-482E-B568-7FE05B9F013C.htm"
generated: "2025-11-28T19:06:49.229470Z"
description: Returns data stored by a specific key/value pair in the Windows Registry or property list file on Mac OS
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

# vl-registry-read (AutoLISP)

> Returns data stored by a specific key/value pair in the Windows Registry or property list file on Mac OS

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B9E26CF-2410-482E-B568-7FE05B9F013C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B9E26CF-2410-482E-B568-7FE05B9F013C.htm)
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
(vl-registry-read
reg-key [val-name]
)
```

- ***reg-key*:** **Type:**  String  Windows registry key or property list file key on Mac OS.
- ***val-name*:** **Type:**  String  Default value for the *reg-key*  entry if it does not exist.

## Return Values

**Type:**  String or nil

A string containing the data stored in the key, if successful; otherwise `nil`.

## Remarks

If *val-name*  is supplied and is not `nil`, the specified value will be read from the registry or property list file. If *val-name*  is absent or `nil`, the function reads the specified key and all of its values.

## Examples

```lisp
(vl-registry-read "HKEY_CURRENT_USER\\Test")

nil

(vl-registry-write "HKEY_CURRENT_USER\\Test" "" "test data")

"test data"

(vl-registry-read "HKEY_CURRENT_USER\\Test")

"test data"
```
