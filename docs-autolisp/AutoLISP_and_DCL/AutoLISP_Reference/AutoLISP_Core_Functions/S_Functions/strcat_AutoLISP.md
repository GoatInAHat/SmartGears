---
title: strcat (AutoLISP)
guid: "GUID-4430B1BF-DBB5-49D1-98F9-711B480976A1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4430B1BF-DBB5-49D1-98F9-711B480976A1.htm"
generated: "2025-11-28T19:06:42.845773Z"
description: Returns a string that is the concatenation of multiple strings
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

# strcat (AutoLISP)

> Returns a string that is the concatenation of multiple strings

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4430B1BF-DBB5-49D1-98F9-711B480976A1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4430B1BF-DBB5-49D1-98F9-711B480976A1.htm)
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
(strcat
[string string_n ...]
)
```

- ***string*:** **Type:**  String  Text values to concatenate.

## Return Values

**Type:**  String

Concatenate text string. If no arguments are supplied, `strcat`  returns a zero-length string.

## Examples

```lisp
(strcat "a" "bout")

"about"

(strcat "a" "b" "c")

"abc"

(strcat "a" "" "c")

"ac"

(strcat)

""
```
