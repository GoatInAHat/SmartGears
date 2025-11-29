---
title: cdr (AutoLISP)
guid: "GUID-F9CD8FF3-022A-4323-BAE7-390174451537"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F9CD8FF3-022A-4323-BAE7-390174451537.htm"
generated: "2025-11-28T19:06:25.385500Z"
description: Returns a list containing all but the first element of the specified list
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

# cdr (AutoLISP)

> Returns a list containing all but the first element of the specified list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F9CD8FF3-022A-4323-BAE7-390174451537.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F9CD8FF3-022A-4323-BAE7-390174451537.htm)
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
(cdr
list
)
```

- ***list*:** **Type:**  List  A list with two or more elements.

## Return Values

**Type:**  Integer, Real, String, List, T or nil

A list containing all the elements of *list,*  except the first element. If the list is empty, `cdr`  returns `nil`.

Note:
 When the
list
 argument is a dotted pair,
cdr
 returns the second element without enclosing it in a list.

## Examples

```lisp
(cdr '(a b c))

(B C)

(cdr '((a b) c))

(C)

(cdr '())

nil

(cdr '(a . b))

B

(cdr '(1 . "Text"))

"Text"
```
