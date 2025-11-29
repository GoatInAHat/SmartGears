---
title: "vl-string-mismatch (AutoLISP)"
guid: "GUID-D1EA6F47-146E-44BC-BBCA-40B7A874C0B8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D1EA6F47-146E-44BC-BBCA-40B7A874C0B8.htm"
generated: "2025-11-28T19:06:50.362047Z"
description: Returns the length of the longest common prefix for two strings, starting at specified positions
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

# vl-string-mismatch (AutoLISP)

> Returns the length of the longest common prefix for two strings, starting at specified positions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D1EA6F47-146E-44BC-BBCA-40B7A874C0B8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D1EA6F47-146E-44BC-BBCA-40B7A874C0B8.htm)
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
(vl-string-mismatch
str1 str2 [pos1 pos2 ignore-case-p]
)
```

- ***str1*:** **Type:**  String  The first textual value to be matched.
- ***str2*:** **Type:**  String  The second textual value to be matched.
- ***pos1*:** **Type:**  Integer  A numeric value identifying the position to search from in the first string; 0 if omitted.
- ***pos2*:** **Type:**  Integer  A numeric value identifying the position to search from in the second string; 0 if omitted.
- ***ignore-case-* p:** **Type:**  T or nil  If `T`  is specified for this argument, case is ignored; otherwise, case is considered.

## Return Values

**Type:**  Integer

A numeric value.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- str1
   and
  str2
   arguments previously accepted ASCII text strings or characters, but these arguments now accept Unicode text strings or characters.
- Return value was modified to support Unicode characters and might be different than earlier releases. In earlier releases, the length of a Unicode character was improperly calculated. For example,
  (vl-string-mismatch "abc中abc" "abc中ñ")
   previously returned 10, but now returns 4.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(vl-string-mismatch "VL-FUN" "VL-VAR")

3

(vl-string-mismatch "vl-fun" "avl-var")

0

(vl-string-mismatch "abc中abc" "abc中ñ")

4

(vl-string-mismatch "€abc" "abc€")

0

(vl-string-mismatch "vl-fun" "avl-var" 0 1)

3

(vl-string-mismatch "VL-FUN" "Vl-vAR")

1

(vl-string-mismatch "VL-FUN" "Vl-vAR" 0 0 T)

3

(vl-string-mismatch "abc€" "ABC€" 0 0 T)

4
```
