---
title: "vl-string-position (AutoLISP)"
guid: "GUID-2B0BF48B-FA26-431B-8EC3-DD0BD9F147A0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2B0BF48B-FA26-431B-8EC3-DD0BD9F147A0.htm"
generated: "2025-11-28T19:06:50.445937Z"
description: Looks for a character with the specified character code in a string
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

# vl-string-position (AutoLISP)

> Looks for a character with the specified character code in a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2B0BF48B-FA26-431B-8EC3-DD0BD9F147A0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2B0BF48B-FA26-431B-8EC3-DD0BD9F147A0.htm)
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
(vl-string-position
char-code str [start-pos [from-end-p]]
)
```

- ***char-code*:** **Type:**  Integer  A numeric value representation of the character to be searched.
- ***str*:** **Type:**  String  The textual value to be searched.
- ***start-pos*:** **Type:**  Integer  The position to begin searching from in the string (first character is 0); 0 if omitted.
- ***from-end-p*:** **Type:**  T or nil  If `T`  is specified for this argument, the search begins at the end of the string and continues backward to *pos*.

## Return Values

**Type:**  Integer or nil

An integer representing the displacement at which *char-code*  was found from the beginning of the string; `nil`  if the character was not found.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- char-code
   argument previously accepted an ASCII character code, but now accepts a Unicode character code.
- str
   argument previously accepted an ASCII text string or character, but now accepts a Unicode text string or character.
- Return value was modified to support Unicode characters and might be different than earlier releases. In earlier releases, the length of a Unicode character was improperly calculated. For example,
  (vl-string-position (ascii "€") "中€" 0)
   previously returned 7, but now returns 1.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(vl-string-position (ascii "z") "azbdc")

1

(vl-string-position 122 "azbzc")

1

(vl-string-position (ascii "x") "azbzc")

nil
```

The search string used in the following example contains two "z" characters. Reading from left to right, with the first character being displacement 0, there is one `z`  at displacement 1 and another `z`  at displacement 3:

```lisp
(vl-string-position (ascii "z") "azbzlmnqc")

1
```

Searching from left to right (the default), the "z" in position 1 is the first one `vl-string-position`  encounters. But when searching from right to left, as in the following example, the "z" in position 3 is the first one encountered:

```lisp
(vl-string-position (ascii "z") "azbzlmnqc" nil T)

3
```
