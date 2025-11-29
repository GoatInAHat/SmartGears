---
title: dictremove (AutoLISP)
guid: "GUID-0A888F13-7C2A-4CB6-B849-56B72EAC972A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0A888F13-7C2A-4CB6-B849-56B72EAC972A.htm"
generated: "2025-11-28T19:06:27.122456Z"
description: Removes an entry from the specified dictionary
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

# dictremove (AutoLISP)

> Removes an entry from the specified dictionary

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0A888F13-7C2A-4CB6-B849-56B72EAC972A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0A888F13-7C2A-4CB6-B849-56B72EAC972A.htm)
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
(dictremove
ename symbol
)
```

- ***ename*:** **Type:**  Ename (entity name)  Name of the dictionary being modified.
- ***symbol*:** **Type:**  String  The entry to be removed from *ename*.

## Return Values

**Type:**  Ename (entity name) or nil

The entity name of the removed entry. If *ename*  is invalid or *symbol*  is not found, `dictremove`  returns `nil.`

## Remarks

By default, removing an entry from a dictionary does not delete it from the database. This must be done with a call to `entdel`. Currently, the exceptions to this rule are groups and multiline styles. The code that implements these features requires that the database and these dictionaries be up to date and, therefore, automatically deletes the entity when it is removed (with `dictremove`) from the dictionary.

The `dictremove`  function does not allow the removal of an multiline style from the multiline style dictionary if it is actively referenced by an multiline in the database.

## Examples

The following example removes the dictionary created in the `dictadd`  example:

```lisp
(dictremove (namedobjdict) "my_way_cool_dictionary")

<Entity name: 1d98950>
```
