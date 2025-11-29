---
title: fix (AutoLISP)
guid: "GUID-93B5F13B-348E-49E0-A116-0861684506D5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-93B5F13B-348E-49E0-A116-0861684506D5.htm"
generated: "2025-11-28T19:06:29.647294Z"
description: Returns the conversion of a real number into the nearest smaller integer
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

# fix (AutoLISP)

> Returns the conversion of a real number into the nearest smaller integer

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-93B5F13B-348E-49E0-A116-0861684506D5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-93B5F13B-348E-49E0-A116-0861684506D5.htm)
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
(fix
number
)
```

- ***number*:** **Type:**  Integer or Real  A numeric value.

## Return Values

**Type:**  Integer

The integer derived from *number*.

If *number*  is larger than the largest possible integer (+2,147,483,647 or -2,147,483,648 on a 32-bit platform), `fix`  returns a truncated real (although integers transferred between AutoLISP and AutoCAD are restricted to 16-bit values).

## Remarks

The `fix` function truncates *number*  to the nearest integer by discarding the fractional portion.

## Examples

```lisp
(fix 3)

3

(fix 3.7)

3
```
