---
title: xdroom (AutoLISP)
guid: "GUID-B7AD1D4B-FAFD-40F2-8A7C-7E35EC23F1FB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B7AD1D4B-FAFD-40F2-8A7C-7E35EC23F1FB.htm"
generated: "2025-11-28T19:06:52.759962Z"
description: Returns the amount of extended data (xdata) space that is available for an object (entity)
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

# xdroom (AutoLISP)

> Returns the amount of extended data (xdata) space that is available for an object (entity)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B7AD1D4B-FAFD-40F2-8A7C-7E35EC23F1FB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B7AD1D4B-FAFD-40F2-8A7C-7E35EC23F1FB.htm)
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
(xdroom
ename
)
```

- ***ename*:** **Type:**  Ename (entity name)  An entity name.

## Return Values

**Type:**  Integer or nil

An integer reflecting the number of bytes of available space. If unsuccessful, `xdroom`  returns `nil`.

## Remarks

Because there is a limit (currently, 16 kilobytes) on the amount of extended data that can be assigned to an entity definition, and because multiple applications can append extended data to the same entity, this function is provided so an application can verify there is room for the extended data that it will append. It can be called in conjunction with `xdsize`, which returns the size of an extended data list.

## Examples

The following example looks up the available space for extended data of a viewport object:

```lisp
(xdroom vpname)

16162
```

In this example, 16,162 bytes of the original 16,383 bytes of extended data space are available, meaning that 221 bytes are used.
