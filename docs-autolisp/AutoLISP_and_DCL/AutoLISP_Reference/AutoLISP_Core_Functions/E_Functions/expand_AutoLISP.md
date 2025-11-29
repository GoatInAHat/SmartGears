---
title: expand (AutoLISP)
guid: "GUID-48C99C97-176D-4D9C-91E2-31B3C67D6EC9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-48C99C97-176D-4D9C-91E2-31B3C67D6EC9.htm"
generated: "2025-11-28T19:06:29.326716Z"
description: Allocates additional memory for AutoLISP
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

# expand (AutoLISP)

> Allocates additional memory for AutoLISP

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-48C99C97-176D-4D9C-91E2-31B3C67D6EC9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-48C99C97-176D-4D9C-91E2-31B3C67D6EC9.htm)
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
(expand
n-expand
)
```

- ***n-expand*:** **Type:**  Integer  An number indicating the amount of additional memory to be allocated. Memory is allocated as follows:  *n-alloc*  free symbols  *n-alloc*  free strings  *n-alloc*  free usubrs  *n-alloc*  free reals  *n-alloc*  * *n-expand*  cons cells  where *n-alloc*  is the current segment size.

## Return Values

**Type:**  Integer

A number indicating the number of free conses divided by *n-alloc*.

## Examples

Set the segment size to 100:

```lisp
(alloc 100)

1000
```

Allocate memory for two additional segments:

```lisp
(expand 2)

82
```

This ensures that AutoLISP now has memory available for at least 200 additional symbols, strings, usubrs and reals each, and 8200 free conses.
