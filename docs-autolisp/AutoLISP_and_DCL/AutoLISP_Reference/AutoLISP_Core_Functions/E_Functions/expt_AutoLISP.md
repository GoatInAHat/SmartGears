---
title: expt (AutoLISP)
guid: "GUID-FCAC3150-7491-43DB-8042-37EDE3791A96"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCAC3150-7491-43DB-8042-37EDE3791A96.htm"
generated: "2025-11-28T19:06:29.397183Z"
description: Returns a number raised to a specified power
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

# expt (AutoLISP)

> Returns a number raised to a specified power

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCAC3150-7491-43DB-8042-37EDE3791A96.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-FCAC3150-7491-43DB-8042-37EDE3791A96.htm)
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
(expt
number power
)
```

- ***number*:** **Type:**  Integer or Real  Any number.
- ***power*:** **Type:**  Integer or Real  The power to raise *number*  to.

## Return Values

**Type:**  Integer or Real

If both arguments are integers, the result is an integer; otherwise, the result is a real.

## Examples

```lisp
(expt 2 4)

16

(expt 3.0 2.0)

9.0
```
