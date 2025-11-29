---
title: "vl-string-trim (AutoLISP)"
guid: "GUID-FF1DA441-C9D8-4C99-BC6E-B2A72B562389"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FF1DA441-C9D8-4C99-BC6E-B2A72B562389.htm"
generated: "2025-11-28T19:06:51.067249Z"
description: Removes the specified characters from the beginning and end of a string
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

# vl-string-trim (AutoLISP)

> Removes the specified characters from the beginning and end of a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FF1DA441-C9D8-4C99-BC6E-B2A72B562389.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FF1DA441-C9D8-4C99-BC6E-B2A72B562389.htm)
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
(vl-string-trim
char-set str
)
```

- ***char-set*:** **Type:**  String  Characters to be removed.
- ***str*:** **Type:**  String  The textual value to be trimmed of *char-set*.

## Return Values

**Type:**  String

The value of *str*, after any characters have been trimmed.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## Examples

```lisp
(vl-string-trim " \t\n" " \t\n STR \n\t ")

"STR"

(vl-string-trim "this is junk" "this is junk Don't call this junk! this is junk")

"Don't call this junk!"

(vl-string-trim " " "      Leave me alone   ")

"Leave me alone"
```
