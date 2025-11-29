---
title: ssgetfirst (AutoLISP)
guid: "GUID-F18CB64C-1B18-46F3-BAD4-24D83214CE0D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F18CB64C-1B18-46F3-BAD4-24D83214CE0D.htm"
generated: "2025-11-28T19:06:42.053617Z"
description: Determines which objects are selected and gripped
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

# ssgetfirst (AutoLISP)

> Determines which objects are selected and gripped

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F18CB64C-1B18-46F3-BAD4-24D83214CE0D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F18CB64C-1B18-46F3-BAD4-24D83214CE0D.htm)
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
(ssgetfirst)
```

No arguments.

## Return Values

**Type:**  List or nil

Returns a list of two selection sets similar to those passed to `sssetfirst`. The first element in the list is always `nil`  because AutoCAD no longer supports grips on unselected objects. The second element is a selection set of entities that are selected and gripped. Both elements of the list can be `nil`.

## Remarks

Note:
 Only entities from the current drawing's model space and paper space, not nongraphical objects or entities in other block definitions, can be analyzed by this function.

## Examples

N/A
