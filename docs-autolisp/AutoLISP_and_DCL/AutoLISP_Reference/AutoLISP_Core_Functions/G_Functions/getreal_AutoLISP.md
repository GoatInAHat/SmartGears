---
title: getreal (AutoLISP)
guid: "GUID-503BBE86-9B22-456A-8883-1B5F5832B7E0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-503BBE86-9B22-456A-8883-1B5F5832B7E0.htm"
generated: "2025-11-28T19:06:31.548808Z"
description: Pauses for user input of a real number, and returns that real number
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

# getreal (AutoLISP)

> Pauses for user input of a real number, and returns that real number

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-503BBE86-9B22-456A-8883-1B5F5832B7E0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-503BBE86-9B22-456A-8883-1B5F5832B7E0.htm)
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
(getreal
[msg]
)
```

- ***msg*:** **Type:**  String  Message to be displayed to prompt the user.

## Return Values

**Type:**  Real or nil

The number entered by the user and expressed as a real.

## Remarks

The user cannot enter another AutoLISP expression as the response to a `getreal`  request.

## Examples

```lisp
(setq val (getreal))
(setq val (getreal "Scale factor: "))
```
