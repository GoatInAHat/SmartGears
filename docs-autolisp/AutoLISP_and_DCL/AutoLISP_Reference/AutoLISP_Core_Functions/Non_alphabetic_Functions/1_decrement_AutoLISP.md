---
title: "1- (decrement) (AutoLISP)"
guid: "GUID-4E3A882A-959D-4B46-BA5E-5A3C1BDB3079"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E3A882A-959D-4B46-BA5E-5A3C1BDB3079.htm"
generated: "2025-11-28T19:06:20.896262Z"
description: Decrements a number by 1
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

# 1- (decrement) (AutoLISP)

> Decrements a number by 1

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E3A882A-959D-4B46-BA5E-5A3C1BDB3079.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4E3A882A-959D-4B46-BA5E-5A3C1BDB3079.htm)
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
(1-
number
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

The *number*  argument, reduced by 1.

## Examples

```lisp
(1- 5)

4

(1- -17.5)

-18.5
```
