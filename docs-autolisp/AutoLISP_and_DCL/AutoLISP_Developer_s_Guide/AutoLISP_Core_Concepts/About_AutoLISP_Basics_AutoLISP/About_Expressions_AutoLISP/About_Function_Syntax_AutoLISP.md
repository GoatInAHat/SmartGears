---
title: About Function Syntax (AutoLISP)
guid: "GUID-2464EBA4-39BC-47F7-95B8-1E6E338823E6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2464EBA4-39BC-47F7-95B8-1E6E338823E6.htm"
generated: "2025-11-28T19:05:59.717561Z"
description: Reference topics in the documentation use a consistent convention to describe the proper syntax for an AutoLISP function.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# About Function Syntax (AutoLISP)

> Reference topics in the documentation use a consistent convention to describe the proper syntax for an AutoLISP function.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2464EBA4-39BC-47F7-95B8-1E6E338823E6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2464EBA4-39BC-47F7-95B8-1E6E338823E6.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The syntax used is as follows:

In this example, the `foo`  function has one required argument, *string*  of the string data type, and one or more optional arguments of numeric value for *number*. The *number*  arguments can be of the integer or real data types. Frequently, the name of the argument indicates the expected data type. The examples in the following table show both valid and invalid calls to the `foo`  function.

| Valid and invalid function call examples |  |
| --- | --- |
| Valid calls | Invalid calls |
| `(foo "catch")` | `(foo 44 13)` |
| `(foo "catch" 22)` | `(foo "fi" "foe" 44 13)` |
| `(foo "catch" 22 31)` | `(foo)` |
