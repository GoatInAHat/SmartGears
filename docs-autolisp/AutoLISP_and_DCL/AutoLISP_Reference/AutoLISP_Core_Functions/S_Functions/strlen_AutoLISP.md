---
title: strlen (AutoLISP)
guid: "GUID-F16AAC5F-5C87-4DC5-A7B9-BDCD25DC507A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F16AAC5F-5C87-4DC5-A7B9-BDCD25DC507A.htm"
generated: "2025-11-28T19:06:42.931444Z"
description: Returns an integer that is the number of characters in a string
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

# strlen (AutoLISP)

> Returns an integer that is the number of characters in a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F16AAC5F-5C87-4DC5-A7B9-BDCD25DC507A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F16AAC5F-5C87-4DC5-A7B9-BDCD25DC507A.htm)
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
(strlen
[str ...]
)
```

- ***str*:** **Type:**  String  A textual value.

## Return Values

**Type:**  Integer

A numeric value. If multiple *str*  arguments are provided, `strlen`  returns the sum of the lengths of all arguments. If you omit the arguments or enter an empty string, `strlen`  returns 0.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- str
   argument previously accepted an ASCII text string or character, but now accepts a Unicode text string or character.
- Return value was modified to support Unicode characters and might be different than earlier releases. In earlier releases, the length of a Unicode character was improperly calculated. For example,
  (strlen "中国")
   previously returned 14, but now returns 2.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  -  1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(strlen "abcd")

4

(strlen "ab")

2

(strlen "one" "two" "four")

10

(strlen "中")

1

(strlen "测试")

2

(strlen)

0

(strlen "")

0
```
