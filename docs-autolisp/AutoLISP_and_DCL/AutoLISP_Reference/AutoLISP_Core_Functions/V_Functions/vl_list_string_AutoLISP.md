---
title: "vl-list->string (AutoLISP)"
guid: "GUID-02C7058A-F648-48F5-BAF6-2A62EABD4DF6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-02C7058A-F648-48F5-BAF6-2A62EABD4DF6.htm"
generated: "2025-11-28T19:06:47.983404Z"
description: Combines the characters associated with a list of integers into a string
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

# vl-list->string (AutoLISP)

> Combines the characters associated with a list of integers into a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-02C7058A-F648-48F5-BAF6-2A62EABD4DF6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-02C7058A-F648-48F5-BAF6-2A62EABD4DF6.htm)
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
(vl-list->string
char-codes-list
)
```

- ***char-codes-list*:** **Type:**  List  A list of non-negative integers. Each integer must be in the range of 1-65536.

## Return Values

**Type:**  String

A textual value consisting of characters, with each character based on one of the integers supplied in *char-codes-list*.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- char-codes-list
   argument previously accepted integers in the range of 1-255, but now accepts integers in the range of 1-65536.
- Return value was modified to support Unicode characters and might be different than earlier releases. For example,
  (vl-list->string (list 49 128))
   previously returned "1€", but now returns "1". If you want to return "1€", your code will need to be updated to
  (vl-list->string (list 49 8364))
  .
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(vl-list->string nil)

""

(vl-list->string '(49 50))

"12"
```
