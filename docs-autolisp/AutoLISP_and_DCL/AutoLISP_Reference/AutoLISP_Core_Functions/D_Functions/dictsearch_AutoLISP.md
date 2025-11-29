---
title: dictsearch (AutoLISP)
guid: "GUID-FCB62959-7C51-41B6-8A0E-1350580F8364"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCB62959-7C51-41B6-8A0E-1350580F8364.htm"
generated: "2025-11-28T19:06:27.370930Z"
description: Searches a dictionary for an item
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

# dictsearch (AutoLISP)

> Searches a dictionary for an item

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCB62959-7C51-41B6-8A0E-1350580F8364.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCB62959-7C51-41B6-8A0E-1350580F8364.htm)
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
(dictsearch
ename symbol [setnext]
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the dictionary being searched.
- ***symbol*:** **Type:**  String  A string that specifies the item to be searched for within the dictionary.
- ***setnext*:** **Type:**  T or nil  If present and not `nil`, the `dictnext`  entry counter is adjusted so the following `dictnext`  call returns the entry after the one returned by this `dictsearch`  call.

## Return Values

**Type:**  List or nil

The entry for the specified item, if successful; otherwise `nil`, if no entry is found.

## Examples

The following example illustrates the use of `dictsearch`  to obtain the dictionary added in the `dictadd`  example:

```lisp
(setq newdictlist (dictsearch (namedobjdict) "my_way_cool_dictionary"))

((-1 . <Entity name: 1d98950>) (0 . "DICTIONARY") (5 . "52") (102 . "{ACAD_REACTORS")
(330 . <Entity name: 1d98860>) (102 . "}") (330 . <Entity name: 1d98860>)
(100 . "AcDbDictionary") (280 . 0) (281 . 1) (3 . "DATA_RECORD_1") (350 . <Entity name: 1d98958>))
```
