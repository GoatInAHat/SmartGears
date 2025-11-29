---
title: strcase (AutoLISP)
guid: "GUID-108DCD2C-6597-4548-856D-937787AFE5E0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-108DCD2C-6597-4548-856D-937787AFE5E0.htm"
generated: "2025-11-28T19:06:42.749187Z"
description: Returns a string where all alphabetic characters have been converted to uppercase or lowercase
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

# strcase (AutoLISP)

> Returns a string where all alphabetic characters have been converted to uppercase or lowercase

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-108DCD2C-6597-4548-856D-937787AFE5E0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-108DCD2C-6597-4548-856D-937787AFE5E0.htm)
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
(strcase
string [which]
)
```

- ***string*:** **Type:**  String  The text string to convert.
- ***which*:** **Type:**  T or nil  If specified as `T`, all alphabetic characters in *string*  are converted to lowercase. Otherwise, characters are converted to uppercase.

## Return Values

**Type:**  String

Converted text value.

## Examples

```lisp
(strcase "Sample")

"SAMPLE"

(strcase "Sample" T)

"sample"
```

The `strcase`  function will correctly handle case mapping of the currently configured character set.
