---
title: read (AutoLISP)
guid: "GUID-5B50BB3E-C244-46E8-85D8-6A2D48B1FE51"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5B50BB3E-C244-46E8-85D8-6A2D48B1FE51.htm"
generated: "2025-11-28T19:06:39.530064Z"
description: Returns the first list or atom obtained from a string
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

# read (AutoLISP)

> Returns the first list or atom obtained from a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5B50BB3E-C244-46E8-85D8-6A2D48B1FE51.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5B50BB3E-C244-46E8-85D8-6A2D48B1FE51.htm)
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
(read
[string]
)
```

- ***string*:** **Type:**  String  A text value. The *string*  argument should not contain blanks, except within a list or string.

## Return Values

**Type:**  Integer, Real, List, Symbol, File, T, or nil

A list or atom. The `read`  function returns its argument converted into the corresponding data type. If no argument is specified, `read`  returns `nil`.

If the string contains multiple AutoLISP expressions separated by AutoLISP symbol delimiters such as blanks, newline, tabs, or parentheses, only the first expression is returned.

## Remarks

The `read`  function parses the string representation of any AutoLISP data and returns the first expression in the string, converting it to a corresponding data type.

## Examples

```lisp
(read "hello")

HELLO

(read "hello there")

HELLO

(read "\"Hi Y'all\"")

"Hi Y'all"

(read "(a b c)")

(A B C)

(read "(a b c) (d)")

(A B C)

(read "1.2300")

1.23

(read "87")

87

(read "87 3.2")

87
```
