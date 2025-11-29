---
title: "vl-string-left-trim (AutoLISP)"
guid: "GUID-24CB6E8B-9F4F-46DD-AB28-6481B86BA82A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-24CB6E8B-9F4F-46DD-AB28-6481B86BA82A.htm"
generated: "2025-11-28T19:06:50.261121Z"
description: Removes the specified characters from the beginning of a string
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

# vl-string-left-trim (AutoLISP)

> Removes the specified characters from the beginning of a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-24CB6E8B-9F4F-46DD-AB28-6481B86BA82A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-24CB6E8B-9F4F-46DD-AB28-6481B86BA82A.htm)
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
(vl-string-left-trim
char-set str
)
```

- ***char-set*:** **Type:**  String  A textual value listing the characters to be removed.
- ***str*:** **Type:**  String  The textual value to be stripped of *char-set*.

## Return Values

**Type:**  String

A textual value containing a substring of *str*  with all leading characters in *char-set*  removed.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## Examples

```lisp
(vl-string-left-trim " \t\n" "\n\t STR ")

"STR "

(vl-string-left-trim "12456789" "12463CPO is not R2D2")

"3CPO is not R2D2"

(vl-string-left-trim " " "     There are too many spaces here")

"There are too many spaces here"
```
