---
title: substr (AutoLISP)
guid: "GUID-36F8701B-7AB7-47BE-AC31-8508A2DF46A2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36F8701B-7AB7-47BE-AC31-8508A2DF46A2.htm"
generated: "2025-11-28T19:06:43.135987Z"
description: Returns a substring of a string
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

# substr (AutoLISP)

> Returns a substring of a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36F8701B-7AB7-47BE-AC31-8508A2DF46A2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36F8701B-7AB7-47BE-AC31-8508A2DF46A2.htm)
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
(substr
str start [length]
)
```

- ***str*:** **Type:**  String  A textual value.
- ***start*:** **Type:**  Integer  A positive numeric value indicating the starting position in *str*. The first character in the string is position 1.
- ***length*:** **Type:**  Integer  A positive numeric value specifying the number of characters to search through in *str*. If *length*  is not specified, the substring continues to the end of *str*.

## Return Values

**Type:**  String

A substring from *str*.

## Remarks

The `substr`  function starts at the *start*  character position of *str*  and continues for *length*  characters.

Note:
 The first character of
str
 is character number 1. This differs from other functions that process elements of a list (like
nth
 and
ssname
) that count the first element as 0.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- str
   argument previously accepted an ASCII text string or character, but now accepts a Unicode text string or character.
- Return value was modified to support Unicode characters and might be different than earlier releases. In earlier releases, the length of a Unicode character was improperly calculated. For example,
  (substr "中国" 1 1)
   previously returned "\\", but now returns "中".
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Example

```lisp
(substr "abcde" 2)

"bcde"

(substr "abcde" 2 1)

"b"

(substr "abcde" 3 2)

"cd"

(substr "€abc€" 4 2)

"c€"

(substr "中国" 1 1)

"中"
```
