---
title: "layerstate-getnames (AutoLISP)"
guid: "GUID-B0EF0543-F792-4391-B54F-606F5685065C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B0EF0543-F792-4391-B54F-606F5685065C.htm"
generated: "2025-11-28T19:06:34.599103Z"
description: Returns a list of the layer state names
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

# layerstate-getnames (AutoLISP)

> Returns a list of the layer state names

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B0EF0543-F792-4391-B54F-606F5685065C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B0EF0543-F792-4391-B54F-606F5685065C.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows and Mac OS only

## Signature

```lisp
(layerstate-getnames
[includehidden] [includexref]
)
```

- ***includehidden*:** **Type:**  T or nil  If *includehidden*  is `T`, the list will include the names of hidden layer states. If omitted or `nil`, hidden layer states will be excluded.
- ***includexref*:** **Type:**  T or nil  If *includexref*  is `nil`, the list will exclude the names of xref layer states. If omitted or `T`, xref layer states will be included

## Return Values

**Type:**  List or nil

Returns a list of the layer state names.

## Examples

```lisp
(layerstate-getnames)

("First Floor" "Second Floor" "Foundation")
```
