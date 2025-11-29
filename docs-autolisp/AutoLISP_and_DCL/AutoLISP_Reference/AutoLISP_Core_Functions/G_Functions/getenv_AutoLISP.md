---
title: getenv (AutoLISP)
guid: "GUID-0796D4FE-C07B-4DB6-9C01-9D772CAFA2C0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0796D4FE-C07B-4DB6-9C01-9D772CAFA2C0.htm"
generated: "2025-11-28T19:06:30.780379Z"
description: Returns the string value assigned to a system environment variable
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

# getenv (AutoLISP)

> Returns the string value assigned to a system environment variable

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0796D4FE-C07B-4DB6-9C01-9D772CAFA2C0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0796D4FE-C07B-4DB6-9C01-9D772CAFA2C0.htm)
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
(getenv
variable-name
)
```

- ***variable-name*:** **Type:**  String  A string specifying the name of the variable to be read. Environment variable names must be spelled and cased exactly as they are stored in the system registry.

## Return Values

**Type:**  String or nil

A string representing the value assigned to the specified system variable. If the variable does not exist, `getenv`  returns `nil`.

## Examples

Assume the system environment variable `ACAD`  is set to */acad/support*  and there is no variable named `NOSUCH`.

```lisp
(getenv "ACAD")

"/acad/support"

(getenv "NOSUCH")

nil
```

Assume that the `MaxArray`  environment variable is set to 10000:

```lisp
(getenv "MaxArray")

"10000"
```
