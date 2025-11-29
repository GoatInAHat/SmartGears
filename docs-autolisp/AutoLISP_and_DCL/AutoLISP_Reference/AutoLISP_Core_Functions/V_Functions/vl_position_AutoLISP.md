---
title: "vl-position (AutoLISP)"
guid: "GUID-E2C60C39-B46E-4779-82CE-CFFCCCE93AF0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E2C60C39-B46E-4779-82CE-CFFCCCE93AF0.htm"
generated: "2025-11-28T19:06:48.751972Z"
description: Returns the index of the specified list item
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

# vl-position (AutoLISP)

> Returns the index of the specified list item

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E2C60C39-B46E-4779-82CE-CFFCCCE93AF0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E2C60C39-B46E-4779-82CE-CFFCCCE93AF0.htm)
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
(vl-position
symbol list
)
```

- ***symbol*:** **Type:**  Integer, Real, String, File, Symbol, Ename (entity name), T, or nil  Any AutoLISP symbol.
- ***list*:** **Type:**  List  A true list.

## Return Values

**Type:**  Integer or nil

A numeric value containing the index position of *symbol*  in *list*; otherwise `nil`  if *symbol*  does not exist in the list.

Note:
 The first list element is index 0, the second element is index 1, and so on.

## Examples

```lisp
(setq stuff (list "a" "b" "c" "d" "e"))

("a" "b" "c" "d" "e")

(vl-position "c" stuff)

2
```
