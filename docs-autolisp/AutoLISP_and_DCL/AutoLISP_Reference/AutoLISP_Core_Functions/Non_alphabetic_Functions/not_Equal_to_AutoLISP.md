---
title: /= (not Equal to) (AutoLISP)
guid: "GUID-AA577C72-FC8C-4697-87F6-FF5CB2F0CBC5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AA577C72-FC8C-4697-87F6-FF5CB2F0CBC5.htm"
generated: "2025-11-28T19:06:20.625175Z"
description: Compares arguments for numerical inequality
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

# /= (not Equal to) (AutoLISP)

> Compares arguments for numerical inequality

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AA577C72-FC8C-4697-87F6-FF5CB2F0CBC5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AA577C72-FC8C-4697-87F6-FF5CB2F0CBC5.htm)
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
(/=
numstr [numstr ...]
)
```

- ***numstr*:** **Type:**  Integer, Real, or String  A number or string.

## Return Values

**Type:**  T or nil

`T`, if no two successive arguments are the same in value; otherwise `nil`. If only one argument is supplied, `T`  is returned.

Note:
 The behavior of
/=
 does not quite conform to other LISP dialects. The standard behavior is to return
T
 if no two arguments in the list have the same value. In AutoLISP,
/=
 returns
T
 if no
successive
 arguments have the same value; see the examples that follow.

## Examples

```lisp
(/= 10 20)

T

(/= "you" "you")

nil

(/= 5.43 5.44)

T

(/= 10 20 10 20 20)

nil

(/= 10 20 10 20)

T
```

Note:
 In the last example, although there are two arguments in the list with the same value, they do not follow one another; thus
/=
 evaluates to
T
.
