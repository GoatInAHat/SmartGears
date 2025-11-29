---
title: solprof (AutoLISP/External Function)
guid: "GUID-142B757C-1A5C-43C1-A011-0698FE0287A7"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-142B757C-1A5C-43C1-A011-0698FE0287A7.htm"
generated: "2025-11-28T19:06:53.728315Z"
description: "Creates profile images of three-dimensional solids"
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

# solprof (AutoLISP/External Function)

> Creates profile images of three-dimensional solids

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-142B757C-1A5C-43C1-A011-0698FE0287A7.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-142B757C-1A5C-43C1-A011-0698FE0287A7.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows and Mac OS only; not available in AutoCAD LT

**Prerequisites:** The AcSolids ObjectARX application must be loaded before the function can be called, `(arxload "acsolids")`. Earlier releases might require you to load the *acsolids.arx*  or *solids.arx*  file.

## Signature

```lisp
(c:solprof
args ...
)
```

- ***args*:** **Type:**  String or Ename (entity name)  The order, number, and type of arguments are the same as those specified when using the AutoCAD SOLPROF command.

## Return Values

**Type:**  nil or error

If successful, `solprof`  returns `nil`; otherwise an error occurs.

## Examples

```lisp
(setq ss (ssget))
(c:solprof ss "y" "y" "n")

nil
```
