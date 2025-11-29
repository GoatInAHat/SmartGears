---
title: acad_strlsort (AutoLISP)
guid: "GUID-760DDC5B-9D89-4464-B695-9FCC963B5C12"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-760DDC5B-9D89-4464-B695-9FCC963B5C12.htm"
generated: "2025-11-28T19:06:21.585862Z"
description: Sorts a list of strings in alphabetical order
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

# acad_strlsort (AutoLISP)

> Sorts a list of strings in alphabetical order

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-760DDC5B-9D89-4464-B695-9FCC963B5C12.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-760DDC5B-9D89-4464-B695-9FCC963B5C12.htm)
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
(acad_strlsort
list
)
```

- ***list*:** **Type:**  List  The list of strings to be sorted.

## Return Values

**Type:**  List or nil

The *list*  in alphabetical order. If the list is invalid or if there is not enough memory to do the sort, `acad_strlsort`  returns `nil`.

## Examples

Sort a list of abbreviated month names:

```lisp
(setq mos '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")

(acad_strlsort mos)

("Apr" "Aug" "Dec" "Feb" "Jan" "Jul" "Jun" "Mar" "May" "Nov" "Oct" "Sep")
```
