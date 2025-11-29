---
title: "vl-catch-all-error-p (AutoLISP)"
guid: "GUID-14CADE53-54C6-437D-8F3C-14845E08591C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-14CADE53-54C6-437D-8F3C-14845E08591C.htm"
generated: "2025-11-28T19:06:45.613875Z"
description: "Determines whether an argument is an error object returned from vl-catch-all-apply"
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

# vl-catch-all-error-p (AutoLISP)

> Determines whether an argument is an error object returned from vl-catch-all-apply

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-14CADE53-54C6-437D-8F3C-14845E08591C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-14CADE53-54C6-437D-8F3C-14845E08591C.htm)
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
(vl-catch-all-error-p
arg
)
```

- ***arg*:** **Type:**  Integer, Real, String, List, Subroutine, Ename (entity name), T, nil, or catch-all-apply-error  Any argument.

## Return Values

**Type:**  T or nil

`T`, if the supplied argument is an error object returned from `vl-catch-all-apply`; otherwise `nil`.

## Examples

Divide by zero using `vl-catch-all-apply`:

```lisp
(setq catchit (vl-catch-all-apply '/ '(50 0)))

#<%catch-all-apply-error%>
```

Use `vl-catch-all-error-p`  to determine if the value returned by `vl-catch-all-apply`  is an error object:

```lisp
(vl-catch-all-error-p catchit)

T
```
