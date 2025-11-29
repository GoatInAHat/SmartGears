---
title: "vl-list-length (AutoLISP)"
guid: "GUID-5BAF3505-B1E9-49BF-9202-AAD36043B6D3"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5BAF3505-B1E9-49BF-9202-AAD36043B6D3.htm"
generated: "2025-11-28T19:06:48.071842Z"
description: Calculates list length of a true list
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

# vl-list-length (AutoLISP)

> Calculates list length of a true list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5BAF3505-B1E9-49BF-9202-AAD36043B6D3.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5BAF3505-B1E9-49BF-9202-AAD36043B6D3.htm)
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
(vl-list-length
list-or-cons-object
)
```

- ***list-or-cons-object*:** **Type:**  List or nil  A true or dotted list.

## Return Values

**Type:**  Integer or nil

An integer containing the list length if the argument is a true list; otherwise `nil`  if *list-or-cons-object*  is a dotted list.

Compatibility note: The `vl-list-length`  function returns `nil`  for a dotted list, while the corresponding Common LISP function issues an error message if the argument is a dotted list.

## Examples

```lisp
(vl-list-length nil)

0

(vl-list-length '(1 2))

2

(vl-list-length '(1 2 . 3))

nil
```
