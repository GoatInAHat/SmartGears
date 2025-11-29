---
title: "acad-pop-dbmod (AutoLISP)"
guid: "GUID-9C141D94-58CD-4A00-8247-7712CDBE813F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C141D94-58CD-4A00-8247-7712CDBE813F.htm"
generated: "2025-11-28T19:06:21.368825Z"
description: "Restores the value of the DBMOD system variable to the value that was most recently stored with acad-push-dbmod"
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

# acad-pop-dbmod (AutoLISP)

> Restores the value of the DBMOD system variable to the value that was most recently stored with acad-push-dbmod

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C141D94-58CD-4A00-8247-7712CDBE813F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9C141D94-58CD-4A00-8247-7712CDBE813F.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

**Prerequisites:**  The AcApp ObjectARX application must be loaded before the function can be called, which is loaded by default.

## Signature

```lisp
(acad-pop-dbmod)
```

 No arguments.

## Return Values

**Type:**  T or nil

Returns `T`  if successful; otherwise, if the stack is empty, returns `nil`.

## Remarks

This function pops the current value of the `DBMOD`  system variable off an internal stack.

This function is used with `acad-push-dbmod`  to control the `DBMOD`  system variable. The `DBMOD`  system variable tracks changes to a drawing and triggers save-drawing queries.
