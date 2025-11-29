---
title: "vl-doc-set (AutoLISP)"
guid: "GUID-F6E10804-16C2-4960-AE45-20C296D991E4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F6E10804-16C2-4960-AE45-20C296D991E4.htm"
generated: "2025-11-28T19:06:46.343368Z"
description: Sets the value of a variable in the current document's namespace
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

# vl-doc-set (AutoLISP)

> Sets the value of a variable in the current document's namespace

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F6E10804-16C2-4960-AE45-20C296D991E4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F6E10804-16C2-4960-AE45-20C296D991E4.htm)
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
(vl-doc-set
'symbol value
)
```

- ***'symbol*:** **Type:**  Symbol  A symbol naming a variable.
- ***value*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  Any value.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, or nil

The *value*  set.

## Remarks

This function can be used by an application to set the value of a variable that resides in the current document's namespace.

If executed within a document namespace, `vl-doc-set`  is equivalent to `set`.

## Examples

```lisp
(vl-doc-set 'foobar "Rinky dinky stinky")

"Rinky dinky stinky"
```
