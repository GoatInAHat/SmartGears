---
title: getcname (AutoLISP)
guid: "GUID-1DCDFFA6-0372-479D-B00F-D165EC5B8A2C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1DCDFFA6-0372-479D-B00F-D165EC5B8A2C.htm"
generated: "2025-11-28T19:06:30.531736Z"
description: Retrieves the localized or English name of an AutoCAD command
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

# getcname (AutoLISP)

> Retrieves the localized or English name of an AutoCAD command

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1DCDFFA6-0372-479D-B00F-D165EC5B8A2C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1DCDFFA6-0372-479D-B00F-D165EC5B8A2C.htm)
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
(getcname
cname
)
```

- ***cname*:** **Type:**  String  The localized or underscored English command name; must be 64 characters or less in length.

## Return Values

If *cname*  is not preceded by an underscore (assumed to be the localized command name), `getcname`  returns the underscored English command name. If *cname*  is preceded by an underscore, `getcname`  returns the localized command name. This function returns `nil`  if *cname*  is not a valid command name.

## Examples

In a French version of AutoCAD, the following is True.

```lisp
(getcname "ETIRER")

"_STRETCH"

(getcname "_STRETCH")

"ETIRER"
```
