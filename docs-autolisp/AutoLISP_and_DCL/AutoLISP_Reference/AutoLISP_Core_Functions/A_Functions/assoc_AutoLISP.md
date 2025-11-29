---
title: assoc (AutoLISP)
guid: "GUID-46309786-DAF6-4C28-8448-599FBC8A4F6A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-46309786-DAF6-4C28-8448-599FBC8A4F6A.htm"
generated: "2025-11-28T19:06:23.883761Z"
description: Searches an association list for an element and returns that association list entry
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

# assoc (AutoLISP)

> Searches an association list for an element and returns that association list entry

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-46309786-DAF6-4C28-8448-599FBC8A4F6A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-46309786-DAF6-4C28-8448-599FBC8A4F6A.htm)
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
(assoc
element alist
)
```

- ***element*:** **Type:**  Integer, Real, or String  Key of an element in an association list.
- ***alist*:** **Type:**  List  An association list to be searched.

## Return Values

**Type:**  List or nil

The *alist*  entry, if successful. If `assoc`  does not find *element*  as a key in *alist*, it returns `nil`.

## Examples

```lisp
(setq al '((name box) (width 3) (size 4.7263) (depth 5)))

((NAME BOX) (WIDTH 3) (SIZE 4.7263) (DEPTH 5))

(assoc 'size al)

(SIZE 4.7263)

(assoc 'weight al)

nil
```
