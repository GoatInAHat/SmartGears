---
title: "vl-registry-delete (AutoLISP)"
guid: "GUID-BF5FB9C5-A015-464C-A475-B320C5B5E96C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BF5FB9C5-A015-464C-A475-B320C5B5E96C.htm"
generated: "2025-11-28T19:06:49.071755Z"
description: Deletes the specified key from the Windows Registry or property list file on Mac OS
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

# vl-registry-delete (AutoLISP)

> Deletes the specified key from the Windows Registry or property list file on Mac OS

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BF5FB9C5-A015-464C-A475-B320C5B5E96C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BF5FB9C5-A015-464C-A475-B320C5B5E96C.htm)
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
(vl-registry-delete
reg-key [val-name]
)
```

- ***reg-key*:** **Type:**  String  Windows registry key or property list file key on Mac OS.
- ***val-name*:** **Type:**  String  Value of the *reg-key*  entry.

## Return Values

**Type:**  T or nil

`T`  if successful; otherwise `nil`.

## Remarks

If *val-name*  is supplied and is not `nil`, the specified value will be purged from the registry or property list file. If *val-name* is absent or `nil`, the function deletes the specified key and all of its values.

## Examples

```lisp
(vl-registry-write "HKEY_CURRENT_USER\\Test" "" "test data")

"test data"

(vl-registry-read "HKEY_CURRENT_USER\\Test")

"test data"

(vl-registry-delete "HKEY_CURRENT_USER\\Test")

T
```

Note:
 This function cannot delete a key that has subkeys. To delete a subkey you must use
vl-registry-descendents
 to enumerate all subkeys and delete all of them.
