---
title: acdimenableupdate (AutoLISP)
guid: "GUID-7E10D18A-6FD6-4B20-A981-27FF77C2F042"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E10D18A-6FD6-4B20-A981-27FF77C2F042.htm"
generated: "2025-11-28T19:06:21.928882Z"
description: Controls the automatic updating of associative dimensions
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

# acdimenableupdate (AutoLISP)

> Controls the automatic updating of associative dimensions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E10D18A-6FD6-4B20-A981-27FF77C2F042.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E10D18A-6FD6-4B20-A981-27FF77C2F042.htm)
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
(acdimenableupdate
flag
)
```

- ***flag*:** **Type:**  T or nil  Controls the automatic updating of associative dimensions when geometry is modified.  **`T`**  -- Enable automatic updating of associative dimensions when the geometry is modified.  **`nil`**  -- Associative dimensions are not updated (even if the geometry is modified) until the DIMREGEN command is used.

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

The `acdimenableupdate`  function is intended for developers who are editing geometry and don't want the dimension to be updated until after the edits are complete.

## Examples

Disable the automatic update of associative dimensions in the drawing:

```lisp
(acdimenableupdate nil)
```

Enable the automatic update of associative dimensions in the drawing:

```lisp
(acdimenableupdate T)
```
