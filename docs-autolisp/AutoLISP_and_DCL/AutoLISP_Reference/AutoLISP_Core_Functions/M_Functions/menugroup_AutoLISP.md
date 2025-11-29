---
title: menugroup (AutoLISP)
guid: "GUID-52FBC08B-F53F-4FC0-A5C5-ED3FA2BB8F4B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-52FBC08B-F53F-4FC0-A5C5-ED3FA2BB8F4B.htm"
generated: "2025-11-28T19:06:37.054406Z"
description: Verifies that a menugroup is loaded
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
---

# menugroup (AutoLISP)

> Verifies that a menugroup is loaded

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-52FBC08B-F53F-4FC0-A5C5-ED3FA2BB8F4B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-52FBC08B-F53F-4FC0-A5C5-ED3FA2BB8F4B.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(menugroup
groupname
)
```

- ***groupname*:** **Type:**  String  Menu group name.

## Return Values

**Type:**  String or nil

If *groupname*  matches a loaded menu group name, the function returns the *groupname*  string; otherwise, it returns `nil`.

Note:

groupname
 is always returned on Mac OS and Web.

## Examples

N/A
