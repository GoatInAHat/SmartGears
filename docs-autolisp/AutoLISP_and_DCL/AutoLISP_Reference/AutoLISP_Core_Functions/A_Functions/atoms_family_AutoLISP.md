---
title: "atoms-family (AutoLISP)"
guid: "GUID-0B35850D-9E62-446B-BAA2-0598A4BDE3D7"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B35850D-9E62-446B-BAA2-0598A4BDE3D7.htm"
generated: "2025-11-28T19:06:24.633562Z"
description: Returns a list of the currently defined symbols
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

# atoms-family (AutoLISP)

> Returns a list of the currently defined symbols

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B35850D-9E62-446B-BAA2-0598A4BDE3D7.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-0B35850D-9E62-446B-BAA2-0598A4BDE3D7.htm)
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
(atoms-family
format [symlist]
)
```

- ***format*:** **Type:**  Integer  An integer value of 0 or 1 that determines the format in which `atoms-family`  returns the symbol names:  **0**  -- Return the symbol names as a list  **1**  -- Return the symbol names as a list of strings
- ***symlist*:** **Type:**  List  A list of strings that specify the symbol names you want `atoms-family`  to search for.

## Return Values

**Type:**  List

A list of symbols. If you specify `symlist`, then `atoms-family`  returns the specified symbols that are currently defined, and returns `nil`  for those symbols that are not defined.

## Examples

```lisp
(atoms-family 0)

(BNS_PRE_SEL FITSTR2LEN C:AI_SPHERE ALERT DEFUN C:BEXTEND REM_GROUP
 B_RESTORE_SYSVARS BNS_CMD_EXIT LISPED FNSPLITL...)
```

The following code verifies that the symbols `CAR`, `CDR`, and `XYZ`  are defined, and returns the list as strings:

```lisp
(atoms-family 1 '("CAR" "CDR" "XYZ"))

("CAR" "CDR" nil)
```

The return value shows that the symbol `XYZ`  is not defined.
