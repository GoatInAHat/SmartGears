---
title: while (AutoLISP)
guid: "GUID-E7C900DB-8B66-4109-BEF6-B0A18E8CF6B6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E7C900DB-8B66-4109-BEF6-B0A18E8CF6B6.htm"
generated: "2025-11-28T19:06:52.324224Z"
description: Evaluates a test expression, and if it is not nil, evaluates other expressions; repeats this process until the test expression evaluates to nil
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

# while (AutoLISP)

> Evaluates a test expression, and if it is not nil, evaluates other expressions; repeats this process until the test expression evaluates to nil

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E7C900DB-8B66-4109-BEF6-B0A18E8CF6B6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-E7C900DB-8B66-4109-BEF6-B0A18E8CF6B6.htm)
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
(while
testexpr [expr ...]
)
```

- ***testexpr*:** **Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil  The expression containing the test condition.
- ***expr*:** **Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil  One or more expressions to be evaluated until *testexpr*  is `nil`.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil

The most recent value of the last *expr*.

## Remarks

The `while`  function continues until *testexpr*  is `nil`.

## Examples

The following code calls user function `some-func`  ten times, with `test`  set to 1 through 10. It then returns 11, which is the value of the last expression evaluated:

```lisp
(setq test 1)
(while (<= test 10)
  (some-func test)
  (setq test (1+ test))
)
```
