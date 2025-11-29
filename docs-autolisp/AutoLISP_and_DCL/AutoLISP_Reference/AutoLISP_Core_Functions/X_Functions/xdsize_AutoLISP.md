---
title: xdsize (AutoLISP)
guid: "GUID-F183738E-43BC-4820-AC51-5D0C1A3DA38C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F183738E-43BC-4820-AC51-5D0C1A3DA38C.htm"
generated: "2025-11-28T19:06:52.845030Z"
description: Returns the size (in bytes) that a list occupies when it is linked to an object (entity) as extended data
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

# xdsize (AutoLISP)

> Returns the size (in bytes) that a list occupies when it is linked to an object (entity) as extended data

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F183738E-43BC-4820-AC51-5D0C1A3DA38C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F183738E-43BC-4820-AC51-5D0C1A3DA38C.htm)
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
(xdsize
lst
)
```

- ***lst*:** **Type:**  List  A valid list of extended data that contains an application name previously registered with the use of the `regapp`  function.

## Return Values

**Type:**  Integer

An integer reflecting the size, in bytes. If unsuccessful, `xdsize`  returns `nil`.

Brace fields (group code 1002) must be balanced. An invalid *lst*  generates an error and places the appropriate error code in the AutoCAD ERRNO system variable. If the extended data contains an unregistered application name, you see this error message (assuming that the AutoCAD CMDECHO system variable is on):

Invalid application name in 1001 group

## Examples

The *lst*  can start with a -3 group code (the extended data sentinel), but it is not required. Because extended data can contain information from multiple applications, the list must have a set of enclosing parentheses.

```lisp
(-3 ("MYAPP" (1000 . "SUITOFARMOR")
             (1002 . "{")
             (1040 . 0.0)
             (1040 . 1.0)
             (1002 . "}")
   )
)
```

Here is the same example without the -3 group code. This list is just the `cdr`  of the first example, but it is important that the enclosing parentheses are included:

```lisp
( ("MYAPP" (1000 . "SUITOFARMOR")
           (1002 . "{")
           (1040 . 0.0)
           (1040 . 1.0)
           (1002 . "}")
   )
)
```
