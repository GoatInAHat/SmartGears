---
title: princ (AutoLISP)
guid: "GUID-7DDA298B-A97C-49C9-94BA-46C77B50AE99"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7DDA298B-A97C-49C9-94BA-46C77B50AE99.htm"
generated: "2025-11-28T19:06:38.783739Z"
description: Prints an expression to the command line, or writes an expression to an open file
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

# princ (AutoLISP)

> Prints an expression to the command line, or writes an expression to an open file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7DDA298B-A97C-49C9-94BA-46C77B50AE99.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7DDA298B-A97C-49C9-94BA-46C77B50AE99.htm)
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
(princ
[expr [file-desc]]
)
```

- ***expr*:** **Type:**  Integer, Real, String, List, Symbol, File, Subroutine, Ename (entity name), T, or nil  A string or AutoLISP expression. Only the specified *expr*  is printed; no newline or space is included.
- ***file-desc*:** **Type:**  File or nil  A file descriptor for a file opened for writing.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil

The value of the evaluated *expr*. If called with no arguments, `princ`  returns a null symbol.

## Remarks

This function is the same as `prin1`, except control characters in *expr*  are printed without expansion. In general, `prin1`  is designed to print expressions in a way that is compatible with `load`, while `princ`  prints them in a way that is readable by functions such as `read-line`.

## Examples

N/A
