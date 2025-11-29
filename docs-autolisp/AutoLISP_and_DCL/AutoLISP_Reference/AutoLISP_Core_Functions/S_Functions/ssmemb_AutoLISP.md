---
title: ssmemb (AutoLISP)
guid: "GUID-8F291146-6A1A-4A31-9380-C800590BF27D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8F291146-6A1A-4A31-9380-C800590BF27D.htm"
generated: "2025-11-28T19:06:42.215236Z"
description: Tests whether an object (entity) is a member of a selection set
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

# ssmemb (AutoLISP)

> Tests whether an object (entity) is a member of a selection set

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8F291146-6A1A-4A31-9380-C800590BF27D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8F291146-6A1A-4A31-9380-C800590BF27D.htm)
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
(ssmemb
ename

ss
)
```

- ***ename*:** **Type:**  Ename (entity name)  An entity name.
- ***ss*:** **Type:**  Pickset (selection set)  A selection set.

## Return Values

**Type:**  Ename (entity name) or nil

If *ename*  is a member of *ss*, `ssmemb`  returns the entity name. If *ename*  is not a member, `ssmemb`  returns `nil`.

## Examples

In the following examples, entity name `e2`  is a member of selection set `ss`, while entity name `e1`  is not a member of `ss`:

```lisp
(ssmemb e2 ss)

<Entity name: 1d62d68>

(ssmemb e1 ss)

nil
```
