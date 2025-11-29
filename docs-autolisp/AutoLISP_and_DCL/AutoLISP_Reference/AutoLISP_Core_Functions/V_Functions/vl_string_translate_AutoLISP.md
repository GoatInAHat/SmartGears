---
title: "vl-string-translate (AutoLISP)"
guid: "GUID-57060085-C79D-4613-B438-506AC443BCE7"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-57060085-C79D-4613-B438-506AC443BCE7.htm"
generated: "2025-11-28T19:06:50.795125Z"
description: Replaces characters in a string with a specified set of characters
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

# vl-string-translate (AutoLISP)

> Replaces characters in a string with a specified set of characters

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-57060085-C79D-4613-B438-506AC443BCE7.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-57060085-C79D-4613-B438-506AC443BCE7.htm)
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
(vl-string-translate
source-set dest-set str
)
```

- ***source-set*:** **Type:**  String  Characters to be matched.
- ***dest-set*:** **Type:**  String  Characters to be substituted for those in *source-set*.
- ***str*:** **Type:**  String  The textual value to be searched and translated.

## Return Values

**Type:**  String

The value of *str*  after any substitutions have been made.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- source-set
  ,
  dest-set
   and
  str
   arguments previously accepted ASCII text strings or characters, but they now accept Unicode text strings or characters.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(vl-string-translate "abcABC" "123123" "A is a, B is b, C is c")

"1 is 1, 2 is 2, 3 is 3"

(vl-string-translate "abc" "123" "A is a, B is b, C is c")

"A is 1, B is 2, C is 3"
```
