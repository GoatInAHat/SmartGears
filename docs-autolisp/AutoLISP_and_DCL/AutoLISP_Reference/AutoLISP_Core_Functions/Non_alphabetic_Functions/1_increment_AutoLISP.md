---
title: 1+ (increment) (AutoLISP)
guid: "GUID-EF06615A-5BE5-4A86-96CF-643B0712E43A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EF06615A-5BE5-4A86-96CF-643B0712E43A.htm"
generated: "2025-11-28T19:06:20.975789Z"
description: Increments a number by 1
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

# 1+ (increment) (AutoLISP)

> Increments a number by 1

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EF06615A-5BE5-4A86-96CF-643B0712E43A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EF06615A-5BE5-4A86-96CF-643B0712E43A.htm)
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
(1+
number
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer or Real

The *number*  argument, increased by 1.

## Examples

```lisp
(1+ 5)

6

(1+ -17.5)

-16.5
```
