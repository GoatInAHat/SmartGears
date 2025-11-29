---
title: untrace (AutoLISP)
guid: "GUID-44FB0D76-668A-449C-9C17-4BCD259577E9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44FB0D76-668A-449C-9C17-4BCD259577E9.htm"
generated: "2025-11-28T19:06:44.638306Z"
description: Clears the trace flag for the specified functions
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

# untrace (AutoLISP)

> Clears the trace flag for the specified functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44FB0D76-668A-449C-9C17-4BCD259577E9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-44FB0D76-668A-449C-9C17-4BCD259577E9.htm)
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
(untrace
[function ...]
)
```

- ***function*:** **Type:**  Symbol  A symbol that names a function. If *function*  is not specified, `untrace`  has no effect.

## Return Values

**Type:**  Symbol or nil

The last function name passed to `untrace`. If *function*  was not specified, `untrace`  returns `nil`.

## Examples

The following command clears the trace flag for function `foo`:

```lisp
(untrace foo)

FOO
```
