---
title: nentselp (AutoLISP)
guid: "GUID-5CE182FE-6455-4C62-B953-B1CA441455C1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5CE182FE-6455-4C62-B953-B1CA441455C1.htm"
generated: "2025-11-28T19:06:37.651117Z"
description: Provides similar functionality to that of the nentsel function without the need for user input
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

# nentselp (AutoLISP)

> Provides similar functionality to that of the nentsel function without the need for user input

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5CE182FE-6455-4C62-B953-B1CA441455C1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5CE182FE-6455-4C62-B953-B1CA441455C1.htm)
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
(nentselp
[msg] [pt]
)
```

- ***msg*:** **Type:**  String  Message to be displayed as a prompt. If the *msg*  argument is omitted, the Select object prompt is issued.
- ***pt*:** **Type:**  List  A selection point. This allows object selection without user input.

## Return Values

**Type:**  List or nil

The `nentselp`  function returns a 4×4 transformation matrix, defined as follows:

The first three columns of the matrix specify scaling and rotation. The fourth column is a translation vector.

The functions that use a matrix of this type treat a point as a column vector of dimension 4. The point is expressed in *homogeneous coordinates,*  where the fourth element of the point vector is a *scale factor*  that is normally set to 1.0. The final row of the matrix, the vector [ *M* ~30~ *M* ~31~ *M* ~32~ *M* ~33~], has the nominal value of [0 0 0 1]; it is currently ignored by the functions that use this matrix format.

## Examples

N/A
