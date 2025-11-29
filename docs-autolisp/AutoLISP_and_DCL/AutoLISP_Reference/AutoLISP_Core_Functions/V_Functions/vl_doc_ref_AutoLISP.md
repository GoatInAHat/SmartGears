---
title: "vl-doc-ref (AutoLISP)"
guid: "GUID-36D04AAD-5555-4A4A-939D-352144FBB1A7"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36D04AAD-5555-4A4A-939D-352144FBB1A7.htm"
generated: "2025-11-28T19:06:46.263586Z"
description: Retrieves the value of a variable from the current document's namespace
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

# vl-doc-ref (AutoLISP)

> Retrieves the value of a variable from the current document's namespace

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36D04AAD-5555-4A4A-939D-352144FBB1A7.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-36D04AAD-5555-4A4A-939D-352144FBB1A7.htm)
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
(vl-doc-ref
'symbol
)
```

- ***'symbol*:** **Type:**  Symbol  A symbol naming a variable.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, or nil

The value of the variable identified by *symbol*.

## Remarks

This function can be used by a separate-namespace application to retrieve the value of a variable from the current document's namespace.

## Examples

```lisp
(vl-doc-ref 'foobar)

"Rinky dinky stinky"
```
