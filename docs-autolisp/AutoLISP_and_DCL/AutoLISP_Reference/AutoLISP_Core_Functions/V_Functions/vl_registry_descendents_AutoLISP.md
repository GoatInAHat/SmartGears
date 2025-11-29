---
title: "vl-registry-descendents (AutoLISP)"
guid: "GUID-3A2A4AA0-8799-4B0A-AD26-443A8B4DD9BE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3A2A4AA0-8799-4B0A-AD26-443A8B4DD9BE.htm"
generated: "2025-11-28T19:06:49.147238Z"
description: Returns a list of subkeys or value names for the specified key of the Windows Registry or property list file on Mac OS
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

# vl-registry-descendents (AutoLISP)

> Returns a list of subkeys or value names for the specified key of the Windows Registry or property list file on Mac OS

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3A2A4AA0-8799-4B0A-AD26-443A8B4DD9BE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-3A2A4AA0-8799-4B0A-AD26-443A8B4DD9BE.htm)
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
(vl-registry-descendents
reg-key [val-names]
)
```

- ***reg-key*:** **Type:**  String  Windows registry key or property list file key on Mac OS.
- ***val-names*:** **Type:**  String  Values for the *reg-key*  entry.

## Return Values

**Type:**  List or nil

A list of strings, if successful; otherwise `nil`.

## Remarks

If *val-names*  is supplied and is not `nil`, the specified value names will be listed from the registry. If *val-name*  is absent or `nil`, the function displays all subkeys of *reg-key*.

## Examples

```lisp
(vl-registry-descendents "HKEY_LOCAL_MACHINE\\SOFTWARE")

("Description" "Program Groups" "ORACLE" "ODBC" "Netscape" "Microsoft")
```
