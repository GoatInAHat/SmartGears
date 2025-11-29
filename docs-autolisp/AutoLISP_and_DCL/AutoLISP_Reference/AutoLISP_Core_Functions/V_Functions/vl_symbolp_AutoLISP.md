---
title: "vl-symbolp (AutoLISP)"
guid: "GUID-F51AFE0A-CA6C-428D-8E8D-3607BF551990"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F51AFE0A-CA6C-428D-8E8D-3607BF551990.htm"
generated: "2025-11-28T19:06:51.705899Z"
description: Identifies whether or not a specified object is a symbol
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

# vl-symbolp (AutoLISP)

> Identifies whether or not a specified object is a symbol

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F51AFE0A-CA6C-428D-8E8D-3607BF551990.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F51AFE0A-CA6C-428D-8E8D-3607BF551990.htm)
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
(vl-symbolp
object
)
```

- ***object*:** **Type:**  Integer, Real, String, List, File, Symbol, Ename (entity name), T, or nil  Any LISP object.

## Return Values

**Type:**  T or nil

`T`  if *object*  is a symbol; otherwise `nil`.

## Examples

```lisp
(vl-symbolp t)

T

(vl-symbolp nil)

nil

(vl-symbolp 1)

nil

(vl-symbolp (list 1))

nil
```
