---
title: alloc (AutoLISP)
guid: "GUID-DBC72668-153D-48F4-9C19-87053975A97C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DBC72668-153D-48F4-9C19-87053975A97C.htm"
generated: "2025-11-28T19:06:22.572165Z"
description: Sets the size of the segment to be used by the expand function
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

# alloc (AutoLISP)

> Sets the size of the segment to be used by the expand function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DBC72668-153D-48F4-9C19-87053975A97C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-DBC72668-153D-48F4-9C19-87053975A97C.htm)
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
(alloc
n-alloc
)
```

- ***n-alloc*:** **Type:**  Integer  A number indicating the amount of memory to be allocated. The integer represents the number of symbols, strings, usubrs, reals, and cons cells.

## Return Values

**Type:**  Integer

The previous setting of `n-alloc`.

## Examples

```lisp
(alloc 100)

1000
```
