---
title: About Symbols and Variables (AutoLISP)
guid: "GUID-1855E7B0-2474-49E6-9EE3-8BC541FC1CC8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1855E7B0-2474-49E6-9EE3-8BC541FC1CC8.htm"
generated: "2025-11-28T19:06:01.734984Z"
description: AutoLISP uses symbols to refer to functions and data holders.
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

# About Symbols and Variables (AutoLISP)

> AutoLISP uses symbols to refer to functions and data holders.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1855E7B0-2474-49E6-9EE3-8BC541FC1CC8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-1855E7B0-2474-49E6-9EE3-8BC541FC1CC8.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Symbol names are not case sensitive and may consist of any sequence of alphanumeric and notation characters, except the following:

| Characters restricted from symbol names |  |
| --- | --- |
| `**(**` | (Open Parenthesis) |
| `**)**` | (Close Parenthesis) |
| `**.**` | (Period) |
| `**'**` | (Apostrophe) |
| `**"**` | (Quote Symbol) |
| `**;**` | (Semicolon) |

A symbol name cannot consist only of numeric characters.

Technically, AutoLISP applications consist of either symbols or constant values, such as strings, reals, and integers. For the sake of clarity, this documentation uses the term symbol to refer to a symbol name that stores static data, such as built-in and user-defined functions. Examples of symbols are, the predefined variable `T`  and the `defun`  function.

The term variable is used to refer to a symbol name that stores program data. The following example uses the `setq`  function to assign the string value "this is a string" to the str1 variable:

```lisp
(setq str1 "this is a string")

"this is a string"
```

Note:
 When declaring variables or defining a function, choose meaningful names to make it easy to identify the type of data a variable holds or the purpose of a function.
