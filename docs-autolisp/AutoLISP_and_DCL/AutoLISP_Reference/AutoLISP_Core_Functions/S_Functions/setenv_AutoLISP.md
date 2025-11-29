---
title: setenv (AutoLISP)
guid: "GUID-0C2E8222-62A8-4529-8A8A-58AAB2A5F23B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0C2E8222-62A8-4529-8A8A-58AAB2A5F23B.htm"
generated: "2025-11-28T19:06:40.447165Z"
description: Sets a system environment variable to a specified value
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

# setenv (AutoLISP)

> Sets a system environment variable to a specified value

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0C2E8222-62A8-4529-8A8A-58AAB2A5F23B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0C2E8222-62A8-4529-8A8A-58AAB2A5F23B.htm)
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
(setenv
varname value
)
```

- ***varname*:** **Type:**  String  Name of the environment variable to be set. Environment variable names must be spelled and cased exactly as they are stored in the system registry.
- ***value*:** **Type:**  String  Value to set *varname*  to.

## Return Values

**Type:**  String

Value provided with the *value*  argument.

## Examples

The following command sets the value of the `MaxArray`  environment variable to 10000:

```lisp
(setenv "MaxArray" "10000")

"10000"
```

Note that changes to settings might not take effect until the next time AutoCAD is started.
