---
title: dictrename (AutoLISP)
guid: "GUID-F99779A7-2A1A-4B04-84B5-C80CC8406321"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F99779A7-2A1A-4B04-84B5-C80CC8406321.htm"
generated: "2025-11-28T19:06:27.216838Z"
description: Renames a dictionary entry
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

# dictrename (AutoLISP)

> Renames a dictionary entry

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F99779A7-2A1A-4B04-84B5-C80CC8406321.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F99779A7-2A1A-4B04-84B5-C80CC8406321.htm)
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
(dictrename
ename oldsym newsym
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the dictionary being modified.
- ***oldsym*:** **Type:**  String  Original key name of the entry.
- ***newsym*:** **Type:**  String  New key name of the entry.

## Return Values

**Type:**  String or nil

The *newsym*  value, if the rename is successful. If the *oldname*  is not present in the dictionary, or if *ename*  or *newname*  is invalid, or if *newname*  is already present in the dictionary, then `dictrename`  returns `nil`.

## Examples

The following example renames the dictionary created in the `dictadd`  sample:

```lisp
(dictrename (namedobjdict) "my_way_cool_dictionary" "An even cooler dictionary")

"An even cooler dictionary"
```
