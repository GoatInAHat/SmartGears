---
title: "vl-registry-write (AutoLISP)"
guid: "GUID-9DDE1962-43C5-40D2-85E3-B7A729CAB157"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9DDE1962-43C5-40D2-85E3-B7A729CAB157.htm"
generated: "2025-11-28T19:06:49.324957Z"
description: Creates a key in the Windows Registry or property list file on Mac OS
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

# vl-registry-write (AutoLISP)

> Creates a key in the Windows Registry or property list file on Mac OS

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9DDE1962-43C5-40D2-85E3-B7A729CAB157.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9DDE1962-43C5-40D2-85E3-B7A729CAB157.htm)
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
(vl-registry-write
reg-key [val-name val-data]
)
```

- ***reg-key*:** **Type:**  String  Windows registry key or property list file key on Mac OS.  Note:  You cannot use `vl-registry-write`  for HKEY_USERS or KEY_LOCAL_MACHINE.
- ***val-name*:** **Type:**  String  Value name for the key.
- ***val-data*:** **Type:**  String  Data for the value name.

## Return Values

**Type:**  String or nil

`vl-registry-write`  returns *val-data*, if successful; otherwise `nil`.

## Remarks

If *val-name*  is not supplied or is `nil`, a default value for the key is written. If *val-name*  is supplied and *val-data*  is not specified, an empty string is stored.

## Examples

```lisp
(vl-registry-write "HKEY_CURRENT_USER\\Test" "" "test data")

"test data"

(vl-registry-read "HKEY_CURRENT_USER\\Test")

"test data"
```
