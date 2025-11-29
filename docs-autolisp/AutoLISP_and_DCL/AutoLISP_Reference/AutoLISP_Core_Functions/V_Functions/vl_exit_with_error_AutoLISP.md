---
title: "vl-exit-with-error (AutoLISP)"
guid: "GUID-718FA582-B7BE-4BA6-8EBF-70B250DF76D8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-718FA582-B7BE-4BA6-8EBF-70B250DF76D8.htm"
generated: "2025-11-28T19:06:46.546290Z"
description: Passes control from an error handler to the *error* function of the calling namespace
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

# vl-exit-with-error (AutoLISP)

> Passes control from an error handler to the *error* function of the calling namespace

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-718FA582-B7BE-4BA6-8EBF-70B250DF76D8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-718FA582-B7BE-4BA6-8EBF-70B250DF76D8.htm)
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
(vl-exit-with-error
msg
)
```

- ***msg*:** **Type:**  String  Message displayed to the user.

## Return Values

**Type:**  None

None

## Remarks

This function is used by applications that run in their own namespace. When `vl-exit-with-error`  executes, it calls the `*error*`  function, the stack is unwound, and control returns to a command prompt.

## Examples

The following code illustrates the use of `vl-exit-with-error`  to pass a string to the `*error*`  function of the calling namespace:

```lisp
(defun *error* (msg)
  ... ; processing in VLX namespace/execution context
(vl-exit-with-error (strcat "My application bombed! " msg)))
```
