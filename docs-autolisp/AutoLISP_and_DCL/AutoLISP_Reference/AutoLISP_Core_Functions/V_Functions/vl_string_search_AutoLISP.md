---
title: "vl-string-search (AutoLISP)"
guid: "GUID-4E7AE1DA-1ED5-4D96-A7D3-34241AA94AA6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E7AE1DA-1ED5-4D96-A7D3-34241AA94AA6.htm"
generated: "2025-11-28T19:06:50.625771Z"
description: Searches for the specified pattern in a string
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

# vl-string-search (AutoLISP)

> Searches for the specified pattern in a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E7AE1DA-1ED5-4D96-A7D3-34241AA94AA6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E7AE1DA-1ED5-4D96-A7D3-34241AA94AA6.htm)
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
(vl-string-search
pattern str [start-pos]
)
```

- ***pattern*:** **Type:**  String  The textual value containing the pattern to be searched for.
- ***str*:** **Type:**  String  The textual value to be searched for *pattern*.
- ***start-pos*:** **Type:**  Integer  A numeric value identifying the starting position of the search; 0 if omitted.

## Return Values

**Type:**  Integer

A numeric value representing the position in the string where the specified *pattern*  was found; otherwise `nil`  if the pattern is not found; the first character of the string is position 0.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- pattern
   and
  str
   arguments previously accepted ASCII text strings or characters, but they now accept Unicode text strings or characters.
- Return value was modified to support Unicode characters and might be different than earlier releases. In earlier releases, the length of a Unicode character was improperly calculated. For example,
  (vl-string-search "€" "€abc中€" 1)
   previously returned 11, but now returns 5.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

```lisp
(vl-string-search "foo" "pfooyey on you")

1

(vl-string-search "who" "pfooyey on you")

nil

(vl-string-search "foo" "fooey-more-fooey" 1)

11

(vl-string-search "€" "€abc中€")

0

(vl-string-search "测试" "€abc中€" 1)

nil

(vl-string-search "€" "€abc中€" 1)

5
```
