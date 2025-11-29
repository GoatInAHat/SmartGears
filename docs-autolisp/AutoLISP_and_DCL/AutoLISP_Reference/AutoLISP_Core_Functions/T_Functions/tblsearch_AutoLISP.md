---
title: tblsearch (AutoLISP)
guid: "GUID-2AEB84A6-E3D0-4DD9-A29C-54D4099ED925"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2AEB84A6-E3D0-4DD9-A29C-54D4099ED925.htm"
generated: "2025-11-28T19:06:43.679626Z"
description: Searches a symbol table for a symbol name
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

# tblsearch (AutoLISP)

> Searches a symbol table for a symbol name

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2AEB84A6-E3D0-4DD9-A29C-54D4099ED925.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2AEB84A6-E3D0-4DD9-A29C-54D4099ED925.htm)
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
(tblsearch
table-name symbol [setnext]
)
```

- ***table-name*:** **Type:**  String  Symbol table to be searched. This argument is not case-sensitive.
- ***symbol*:** **Type:**  String  Symbol name to be searched for. This argument is not case-sensitive.
- ***setnext*:** **Type:**  T or nil  If this argument is supplied and is not `nil`, the `tblnext`  entry counter is adjusted so the following `tblnext`  call returns the entry after the one returned by this `tblsearch`  call. Otherwise, `tblsearch`  has no effect on the order of entries retrieved by `tblnext`.

## Return Values

**Type:**  List (dotted pairs) or nil

If `tblsearch`  finds an entry for the given symbol name, it returns that entry. If no entry is found, `tblsearch`  returns `nil`.

## Examples

The following command searches for a text style named “standard”:

```lisp
(tblsearch "style" "standard")

((0 . "STYLE") (2 . "STANDARD") (70 . 0) (40 . 0.0) (41 . 1.0) (50 . 0.0) (71 . 0) (42 . 0.3) (3 . "txt") (4 . ""))
```
