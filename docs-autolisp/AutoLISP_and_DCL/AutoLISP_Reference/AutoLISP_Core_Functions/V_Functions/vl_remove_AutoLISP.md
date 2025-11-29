---
title: "vl-remove (AutoLISP)"
guid: "GUID-1BF33827-5C9C-49B2-A21B-656E0F429B21"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1BF33827-5C9C-49B2-A21B-656E0F429B21.htm"
generated: "2025-11-28T19:06:49.427576Z"
description: Removes elements from a list
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

# vl-remove (AutoLISP)

> Removes elements from a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1BF33827-5C9C-49B2-A21B-656E0F429B21.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1BF33827-5C9C-49B2-A21B-656E0F429B21.htm)
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
(vl-remove
element-to-remove lst
)
```

- ***element-to-remove*:** **Type:**  Integer, Real, String, List, File, Ename (entity name), T, or nil  The value of the element to be removed; may be any LISP data type.
- ***lst*:** **Type:**  List  Any list.

## Return Values

**Type:**  List or nil

The *lst*  with all elements except those equal to *element-to-remove*.

## Examples

```lisp
(vl-remove pi (list pi t 0 "abc"))

(T 0 "abc")
```
