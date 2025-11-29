---
title: load (AutoLISP)
guid: "GUID-F3639BAA-FD70-487C-AEB5-9E6096EC0255"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F3639BAA-FD70-487C-AEB5-9E6096EC0255.htm"
generated: "2025-11-28T19:06:36.076568Z"
description: Evaluates the AutoLISP expressions in a file
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

# load (AutoLISP)

> Evaluates the AutoLISP expressions in a file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F3639BAA-FD70-487C-AEB5-9E6096EC0255.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F3639BAA-FD70-487C-AEB5-9E6096EC0255.htm)
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
(load
filename [onfailure]
)
```

- ***filename*:** **Type:**  String  Name of the AutoLISP file to load. If the `filename`  argument does not specify a file extension, `load`  adds an extension to the name when searching for a file to load. The function will try several extensions, if necessary, in the following order:  *.vlx*  *.fas*  . *lsp*  Note:  VLX files are supported on Windows only.  As soon as `load`  finds a match, it stops searching and loads the file.  The `filename`  can include a directory prefix, as in *c:/function/test1*  (Windows) or */function/test1*  (Mac OS and Web). A forward slash (`/`) or two backslashes (`\\`) are valid directory delimiters. If you don't include a directory prefix in the `filename`  string, `load`  searches the AutoCAD library path for the specified file. If the file is found anywhere on this path, `load`  then loads the file.
- ***onfailure*:** **Type:**  String  A value returned if `load`  fails.  If the *onfailure*  argument is a valid AutoLISP function, it is evaluated. In most cases, the *onfailure*  argument should be a string or an atom. This allows an AutoLISP application calling `load`  to take alternative action upon failure.

## Remarks

The `load`  function can be used from within another AutoLISP function, or even recursively (in the file being loaded).

Important:
 Starting with AutoCAD 2014-based products, custom applications must work under secure mode; when the SECURELOAD system variable is set to 1 or 2. When operating under secure mode, the program is restricted to loading and executing files that contain code from trusted locations; trusted locations are specified by the TRUSTEDPATHS system variable.

## Return Values

**Type:**  String, Subroutine, or Error

Unspecified, if successful. If `load`  fails, it returns the value of *onfailure*; if *onfailure*  is not defined, failure results in an error message.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- filename
   argument previously accepted an ASCII text string, but now accepts a Unicode text string.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

For the following examples, assume that file */fred/test1.lsp*  contains the expressions

```lisp
(defun MY-FUNC1 (x)

          ...

function body

...

)
(defun MY-FUNC2 (x)

          ...

function body

...

)
```

and that no file named *test2*  with a *.lsp*, *.fas*, or *.vlx*  extension exists:

```lisp
(load "/fred/test1")

MY-FUNC2

(load "\\fred\\test1")

MY-FUNC2

(load "/fred/test1" "bad")

MY-FUNC2

(load "test2" "bad")

"bad"

(load "test2")

; error: LOAD failed: "test2"
```
