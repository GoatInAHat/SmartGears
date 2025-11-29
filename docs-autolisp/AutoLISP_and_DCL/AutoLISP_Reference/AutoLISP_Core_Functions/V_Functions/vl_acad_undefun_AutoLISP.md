---
title: "vl-acad-undefun (AutoLISP)"
guid: "GUID-81CEB01F-8A57-4465-A4F9-CE2E357D692B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-81CEB01F-8A57-4465-A4F9-CE2E357D692B.htm"
generated: "2025-11-28T19:06:44.938966Z"
description: Undefines an AutoLISP function symbol so it is no longer available to ObjectARX applications
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

# vl-acad-undefun (AutoLISP)

> Undefines an AutoLISP function symbol so it is no longer available to ObjectARX applications

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-81CEB01F-8A57-4465-A4F9-CE2E357D692B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-81CEB01F-8A57-4465-A4F9-CE2E357D692B.htm)
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
(vl-acad-undefun
'symbol
)
```

- **symbol:** **Type:**  Subroutine or Symbol  A symbol identifying a function.

## Return Values

**Type:**  Integer or nil

A numeric value if successful; `nil`  if unsuccessful (for example, the function was not defined in AutoLISP).

## Remarks

You can use `vl-acad-undefun`  to undefine a `c:`  function or a function that was exposed by `vl-acad-defun`.

## Examples

None
