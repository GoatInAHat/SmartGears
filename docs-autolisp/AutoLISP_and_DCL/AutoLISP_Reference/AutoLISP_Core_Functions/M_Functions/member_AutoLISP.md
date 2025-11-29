---
title: member (AutoLISP)
guid: "GUID-A2B08751-D966-44F5-9B02-1AAC4DA6AF59"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A2B08751-D966-44F5-9B02-1AAC4DA6AF59.htm"
generated: "2025-11-28T19:06:36.854610Z"
description: Searches a list for an occurrence of an expression and returns the remainder of the list, starting with the first occurrence of the expression
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

# member (AutoLISP)

> Searches a list for an occurrence of an expression and returns the remainder of the list, starting with the first occurrence of the expression

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A2B08751-D966-44F5-9B02-1AAC4DA6AF59.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-A2B08751-D966-44F5-9B02-1AAC4DA6AF59.htm)
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
(member
expr lst
)
```

- ***expr*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  The expression to be searched for.
- ***lst*:** **Type:**  List  The list in which to search for *expr*.

## Return Values

**Type:**  List or nil

A list; otherwise `nil`, if there is no occurrence of *expr*  in *lst*.

## Examples

```lisp
(member 'c '(a b c d e))

(C D E)

(member 'q '(a b c d e))

nil
```
