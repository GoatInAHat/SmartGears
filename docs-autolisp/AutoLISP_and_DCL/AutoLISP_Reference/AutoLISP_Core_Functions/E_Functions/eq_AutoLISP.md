---
title: eq (AutoLISP)
guid: "GUID-BC81B391-48BF-401E-80D0-05A1B578A0FF"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BC81B391-48BF-401E-80D0-05A1B578A0FF.htm"
generated: "2025-11-28T19:06:28.740833Z"
description: Determines whether two expressions are identical
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

# eq (AutoLISP)

> Determines whether two expressions are identical

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BC81B391-48BF-401E-80D0-05A1B578A0FF.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BC81B391-48BF-401E-80D0-05A1B578A0FF.htm)
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
(eq
expr1 expr2
)
```

- ***expr1*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  The expression to be compared.
- ***expr2*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  The expression to compare with *expr1*.

## Return Values

**Type:**  T or nil

`T`  if the two expressions are identical; otherwise `nil`.

## Remarks

The `eq`  function determines whether *expr1*  and *expr2*  are bound to the same object (by `setq`, for example).

## Examples

Given the following assignments:

```lisp
(setq f1 '(a b c))
(setq f2 '(a b c))
(setq f3 f2)
```

Compare `f1`  and `f3`:

```lisp
(eq f1 f3)

nil
```

`eq`  returns `nil`  because `f1`  and `f3`, while containing the same value, do not refer to the same list.

Compare `f3`  and `f2`:

```lisp
(eq f3 f2)

T
```

`eq`  returns `T`  because `f3`  and `f2`  refer to the same list.
