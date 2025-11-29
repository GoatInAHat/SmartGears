---
title: apply (AutoLISP)
guid: "GUID-0574ADA0-0950-456A-9330-A2518421536E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0574ADA0-0950-456A-9330-A2518421536E.htm"
generated: "2025-11-28T19:06:23.113493Z"
description: Passes a list of arguments to, and executes, a specified function
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

# apply (AutoLISP)

> Passes a list of arguments to, and executes, a specified function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0574ADA0-0950-456A-9330-A2518421536E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0574ADA0-0950-456A-9330-A2518421536E.htm)
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
(apply
'function list
)
```

- ***'function*:** **Type:**  Symbol  A function.  The *function*  argument can be either a symbol identifying a `defun`, or a `lambda`  expression.
- ***list*:** **Type:**  List or nil  A list.  If the function accepts no arguments, the value can be `nil`.

## Return Values

**Type:**  String, Integer, Real, List, T, or nil

The result of the function call.

## Examples

```lisp
(apply '+ '(1 2 3))

6

(apply 'strcat '("a" "b" "c"))

"abc"
```
