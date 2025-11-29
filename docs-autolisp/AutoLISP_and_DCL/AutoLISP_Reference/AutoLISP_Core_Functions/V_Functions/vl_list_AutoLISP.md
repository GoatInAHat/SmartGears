---
title: "vl-list* (AutoLISP)"
guid: "GUID-959DE11B-95E8-4AC3-BFBD-02406977D343"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-959DE11B-95E8-4AC3-BFBD-02406977D343.htm"
generated: "2025-11-28T19:06:47.896609Z"
description: Constructs and returns a list
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

# vl-list* (AutoLISP)

> Constructs and returns a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-959DE11B-95E8-4AC3-BFBD-02406977D343.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-959DE11B-95E8-4AC3-BFBD-02406977D343.htm)
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
(vl-list*
object [object ...]
)
```

- ***object*:** **Type:**  Integer, Real, String, List, File, Symbol, Ename (entity name), T, or nil  Any LISP object.

## Return Values

The `vl-list*`  function is similar to `list`, but it will place the last *object*  in the final `cdr`  of the result list. If the last argument to `vl-list*`  is an atom, the result is a dotted list. If the last argument is a list, its elements are appended to all previous arguments added to the constructed list. The possible return values from `vl-list*`  are

- An atom, if a single atom
  object
   is specified.
- A dotted pair, if all
  object
   arguments are atoms.
- A dotted list, if the last argument is an atom and neither of the previous conditions is true.
- A list, if none of the previous statements is true.

## Examples

```lisp
(vl-list* 1)

1

(vl-list* 0 "text")

(0 . "TEXT")

(vl-list* 1 2 3)

(1 2 . 3)

(vl-list* 1 2 '(3 4))

(1 2 3 4)
```
