---
title: tblobjname (AutoLISP)
guid: "GUID-B75D58C9-8ED9-4025-906A-76B243CD5D8D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B75D58C9-8ED9-4025-906A-76B243CD5D8D.htm"
generated: "2025-11-28T19:06:43.533357Z"
description: Returns the entity name of a specified symbol table entry
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

# tblobjname (AutoLISP)

> Returns the entity name of a specified symbol table entry

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B75D58C9-8ED9-4025-906A-76B243CD5D8D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B75D58C9-8ED9-4025-906A-76B243CD5D8D.htm)
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
(tblobjname
table-name symbol
)
```

- ***table-name*:** **Type:**  String  Symbol table to be searched. The argument is not case-sensitive.
- ***symbol*:** **Type:**  String  Symbol to be searched for.

## Return Values

**Type:**  Ename (entity name) or nil

The entity name of the symbol table entry, if found.

The entity name returned by `tblobjname`  can be used in `entget`  and `entmod`  operations.

## Examples

The following command searches for the entity name of the block entry “ESC-01”:

```lisp
(tblobjname "block" "ESC-01")

<Entity name: 1dca368>
```
