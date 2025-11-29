---
title: "vl-string-subst (AutoLISP)"
guid: "GUID-D8EE91DC-D4DB-43E0-9AFE-5FA166C0896D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D8EE91DC-D4DB-43E0-9AFE-5FA166C0896D.htm"
generated: "2025-11-28T19:06:50.700574Z"
description: Substitutes one string for another, within a string
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

# vl-string-subst (AutoLISP)

> Substitutes one string for another, within a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D8EE91DC-D4DB-43E0-9AFE-5FA166C0896D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D8EE91DC-D4DB-43E0-9AFE-5FA166C0896D.htm)
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
(vl-string-subst
new-str pattern str [start-pos]
)
```

- ***new-str*:** **Type:**  String  The textual value to be substituted for *pattern*.
- ***pattern*:** **Type:**  String  The textual value containing the pattern to be replaced.
- ***str*:** **Type:**  String  The textual value to be searched for *pattern*.
- ***start-pos*:** **Type:**  Integer  A numeric value identifying the starting position of the search; 0 if omitted.

## Return Values

**Type:**  String

The value of *str*  after any substitutions have been made.

## Remarks

Note that the search is case-sensitive, and that `vl-string-subst`  substitutes only the first occurrence it finds of the string.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- new-str
  ,
  pattern
   and
  str
   arguments previously accepted ASCII text strings or characters, but now accept Unicode text strings or characters.
- Return value was modified to support Unicode characters and might be different than earlier releases. In earlier releases, the length of a Unicode character was improperly calculated. For example,
  (vl-string-subst "中" "€" "abc中€bec" 5)
   previously returned "abc中中bec", but now returns "abc中€bec".
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

Replace the string "Ben" with "Obi-wan":

```lisp
(vl-string-subst "Obi-wan" "Ben" "Ben Kenobi")

"Obi-wan Kenobi"
```

Replace "Ben" with "Obi-wan":

```lisp
(vl-string-subst "Obi-wan" "Ben" "ben Kenobi")

"ben Kenobi"
```

Nothing was substituted because `vl-string-subst`  did not find a match for "Ben"; the "ben" in the string that was searched begins with a lowercase "b".

Replace "Ben" with "Obi-wan":

```lisp
(vl-string-subst "Obi-wan" "Ben" "Ben Kenobi Ben")

"Obi-wan Kenobi Ben"
```

Note that there are two occurrences of "Ben" in the string that was searched, but `vl-string-subst`  replaces only the first occurrence.

Replace "Ben" with "Obi-wan," but start the search at the fourth character in the string:

```lisp
(vl-string-subst "Obi-wan" "Ben" "Ben \"Ben\" Kenobi" 3)

"Ben \"Obi-wan\" Kenobi"
```

There are two occurrences of "Ben" in the string that was searched, but because `vl-string-subst`  was instructed to begin searching at the fourth character, it found and replaced the second occurrence, not the first.
