---
title: "vl-catch-all-error-message (AutoLISP)"
guid: "GUID-C703867D-7B64-480D-BE34-45A63BA02AB7"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C703867D-7B64-480D-BE34-45A63BA02AB7.htm"
generated: "2025-11-28T19:06:45.402403Z"
description: Returns a string from an error object
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

# vl-catch-all-error-message (AutoLISP)

> Returns a string from an error object

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C703867D-7B64-480D-BE34-45A63BA02AB7.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C703867D-7B64-480D-BE34-45A63BA02AB7.htm)
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
(vl-catch-all-error-message
error-obj
)
```

- ***error-obj*:** **Type:**  catch-all-apply-error  An error object returned by `vl-catch-all-apply`.

## Return Values

**Type:**  String

A textual value containing an error message.

## Examples

Divide by zero using `vl-catch-all-apply`:

```lisp
(setq catchit (vl-catch-all-apply '/ '(50 0)))

#<%catch-all-apply-error%>
```

The `vl-catch-all-apply`  function traps the error and returns an error object. Use `vl-catch-all-error-message`  to see the error message contained in the error object:

```lisp
(vl-catch-all-error-message catchit)

"divide by zero"
```
