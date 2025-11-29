---
title: sslength (AutoLISP)
guid: "GUID-034B9EC8-0945-48A0-802A-9725DDBA0EF2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-034B9EC8-0945-48A0-802A-9725DDBA0EF2.htm"
generated: "2025-11-28T19:06:42.137025Z"
description: Returns an integer containing the number of objects (entities) in a selection set
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

# sslength (AutoLISP)

> Returns an integer containing the number of objects (entities) in a selection set

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-034B9EC8-0945-48A0-802A-9725DDBA0EF2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-034B9EC8-0945-48A0-802A-9725DDBA0EF2.htm)
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
(sslength
ss
)
```

- ***ss*:** **Type:**  Pickset (selection set)  A selection set.

## Return Values

**Type:**  Integer

The number of objects in the selection set.

## Examples

Add the last object to a new selection set:

```lisp
(setq sset (ssget "L"))

<Selection set: 8>
```

Use `sslength`  to determine the number of objects in the new selection set:

```lisp
(sslength sset)

1
```
