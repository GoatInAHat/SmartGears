---
title: equal (AutoLISP)
guid: "GUID-7E85CB8F-B4DA-42F3-ABD3-89342A11EF9B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E85CB8F-B4DA-42F3-ABD3-89342A11EF9B.htm"
generated: "2025-11-28T19:06:28.835671Z"
description: Determines whether two expressions are equal
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

# equal (AutoLISP)

> Determines whether two expressions are equal

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E85CB8F-B4DA-42F3-ABD3-89342A11EF9B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E85CB8F-B4DA-42F3-ABD3-89342A11EF9B.htm)
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
(equal
expr1 expr2 [fuzz]
)
```

- ***expr1*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  The expression to be compared.
- ***expr2*:** **Type:**  Integer, Real, String, List, Ename (entity name), T, or nil  The expression to compare with *expr1*.
- ***fuzz*:** **Type:**  Integer or Real  A real number defining the maximum amount by which *expr1*  and *expr2*  can differ and still be considered equal.

## Return Values

**Type:**  T or nil

`T`  if the two expressions are equal (evaluate to the same value); otherwise `nil`.

## Remarks

When comparing two real numbers (or two lists of real numbers, as in points), the two identical numbers can differ slightly if different methods are used to calculate them. You can specify a *fuzz*  amount to compensate for the difference that may result from the different methods of calculation.

- **Comparing the `eq`  and `equal`  Functions:** If the `eq`  function finds that two lists or atoms are the same, the `equal`  function also finds them to be the same.  Any *atoms*  that the `equal`  function determines to be the same are also found equivalent by `eq`. However, two *lists*  that `equal`  determines to be the same may be found to be different according to the `eq`  function.

## Examples

Given the following assignments:

```lisp
(setq f1 '(a b c))
(setq f2 '(a b c))
(setq f3 f2)
(setq a 1.123456)
(setq b 1.123457)
```

Compare `f1`  to `f3`:

```lisp
(equal f1 f3)

T
```

Compare `f3`  to `f2`:

```lisp
(equal f3 f2)

T
```

Compare `a`  to `b`:

```lisp
(equal a b)

nil
```

The `a`  and `b`  variables differ by .000001. Compare `a`  to `b`:, with *fuzz*  argument of .000001:

```lisp
(equal a b 0.000001)

T
```

The `a`  and `b`  variables differ by an amount equal to the specified *fuzz*  factor, so `equal`  considers the variables equal.
