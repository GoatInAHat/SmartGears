---
title: "acet-layerp-mode (AutoLISP)"
guid: "GUID-6E9FD664-C724-48BA-BFB7-8A28D02B59B5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6E9FD664-C724-48BA-BFB7-8A28D02B59B5.htm"
generated: "2025-11-28T19:06:22.077920Z"
description: Queries and sets the LAYERPMODE setting
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

# acet-layerp-mode (AutoLISP)

> Queries and sets the LAYERPMODE setting

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6E9FD664-C724-48BA-BFB7-8A28D02B59B5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6E9FD664-C724-48BA-BFB7-8A28D02B59B5.htm)
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
(acet-layerp-mode
[status]
)
```

- ***status*:** **Type:**  T or nil  `T`  - Turns LAYERPMODE on, enabling layer-change tracking  `nil`  - Turns LAYERPMODE off  If this argument is not present, `acet-layerp-mode`  returns the current status of LAYERPMODE.

## Return Values

**Type:**  T or nil

`T`  if current status of LAYERPMODE is on; `nil`  if LAYERPMODE is off.

## Examples

Check the current status of LAYERPMODE:

```lisp
(acet-layerp-mode)

T
```

Turn LAYERPMODE off:

```lisp
(acet-layerp-mode nil)

nil
```

Check the current status of LAYERPMODE:

```lisp
(acet-layerp-mode)

nil
```
