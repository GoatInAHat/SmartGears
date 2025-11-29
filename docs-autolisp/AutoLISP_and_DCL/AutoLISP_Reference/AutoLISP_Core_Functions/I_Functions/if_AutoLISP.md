---
title: if (AutoLISP)
guid: "GUID-916F1A5C-FD70-4D66-897E-6DCD666DCB39"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-916F1A5C-FD70-4D66-897E-6DCD666DCB39.htm"
generated: "2025-11-28T19:06:32.817410Z"
description: Conditionally evaluates expressions
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

# if (AutoLISP)

> Conditionally evaluates expressions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-916F1A5C-FD70-4D66-897E-6DCD666DCB39.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-916F1A5C-FD70-4D66-897E-6DCD666DCB39.htm)
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
(if
testexpr thenexpr [elseexpr]
)
```

- ***testexpr*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  Expression to be tested.
- ***thenexpr*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  Expression evaluated if *testexpr*  is not `nil`.
- ***elseexpr*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  Expression evaluated if *testexpr*  is `nil`.

## Return Values

**Type:**  Integer, Real, String, List, Ename (entity name), T, or nil

The `if`  function returns the value of the selected expression. If *elseexpr*  is missing and *testexpr*  is `nil`, then it returns `nil`.

## Examples

```lisp
(if (= 1 3) "YES!!" "no.")

"no."

(if (= 2 (+ 1 1)) "YES!!")

"YES!!"

(if (= 2 (+ 3 4)) "YES!!")

nil
```
