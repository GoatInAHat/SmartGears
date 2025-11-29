---
title: set (AutoLISP)
guid: "GUID-041BE20B-CFA6-465B-A98B-1BCBDC810881"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-041BE20B-CFA6-465B-A98B-1BCBDC810881.htm"
generated: "2025-11-28T19:06:40.260333Z"
description: Sets the value of a quoted symbol name to an expression
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

# set (AutoLISP)

> Sets the value of a quoted symbol name to an expression

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-041BE20B-CFA6-465B-A98B-1BCBDC810881.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-041BE20B-CFA6-465B-A98B-1BCBDC810881.htm)
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
(set
sym expr
)
```

- ***sym*:** **Type:**  Symbol  The user-defined variable to assign *expr*  to.
- ***expr*:** **Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil  An AutoLISP expression.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil

The value of the expression.

## Remarks

The `set`  function is similar to `setq`  except that `set`  evaluates both of its arguments whereas `setq`  only evaluates its second argument.

## Examples

Each of the following commands sets symbol `a`  to 5.0:

```lisp
(set 'a 5.0)

5.0

(set (read "a") 5.0)

5.0

(setq a 5.0)

5.0
```

Both `set`  and `setq`  expect a symbol as their first argument, but `set`  accepts an expression that returns a symbol, whereas `setq`  does not, as the following shows:

```lisp
(set (read "a") 5.0)

5.0

(setq (read "a") 5.0)

; *** ERROR: syntax error
```
