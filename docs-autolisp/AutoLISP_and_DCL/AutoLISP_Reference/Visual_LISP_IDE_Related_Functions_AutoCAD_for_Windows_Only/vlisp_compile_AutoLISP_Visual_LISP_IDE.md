---
title: "vlisp-compile (AutoLISP/Visual LISP IDE)"
guid: "GUID-8184531C-EDE5-4949-9EAE-25CE98BFF89C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8184531C-EDE5-4949-9EAE-25CE98BFF89C.htm"
generated: "2025-11-28T19:06:54.099296Z"
description: Compiles AutoLISP source code into a FAS file
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

# vlisp-compile (AutoLISP/Visual LISP IDE)

> Compiles AutoLISP source code into a FAS file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8184531C-EDE5-4949-9EAE-25CE98BFF89C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-8184531C-EDE5-4949-9EAE-25CE98BFF89C.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  AutoCAD for Windows only; not available in AutoCAD LT for Windows

## Signature

```lisp
(vlisp-compile '
mode filename [output-filename]
)
```

- ***mode*:** **Type:**  Symbol  The compiler mode, which can be one of the following symbols:  **st**  Standard build mode - Produces the smallest output file and is suitable for programs consisting of a single file.  **lsm**  Optimize and link indirectly - Optimizes the compiled files, but does not create direct references to the compiled functions in the compiled code.  **lsa**  Optimize and link directly - Optimizes the compiled files and creates direct references to the compiled function in the compiled code, instead of to the function symbol.  Note:  Both the optimization options are best suited for large and complex programs.  The basic functions of optimization are as follows:  Link function calls to create direct references to the compiled function in the compiled code, instead of to the function symbol. This feature improves the performance of the compiled code and protects the code against function redefinition at runtime.  Drop function names to make the compiled code more secure and to decrease program size and load time.  Drop the names of all local variables and directly link their references. This also makes the compiled code more secure and decreases program size and load time.
- ***filename*:** **Type:**  String  AutoLISP source file name. If the source file is in the AutoCAD support file search path, you can omit the path when specifying the file name. If you omit the file extension, *.lsp* is assumed.
- ***output-filename*:** **Type:**  String  Compiled output file name. If you do not specify an output file, `vlisp-compile`  names the output with the same name as the input file, but replaces the extension with *.fas*.  Note:  If you specify an output file name but do not specify a path name for either the input or the output file, `vlisp-compile`  places the output file in the AutoCAD installation directory.

## Return Values

**Type:**  T or nil

`T`, if compilation is successful; otherwise `nil`.

## Remarks

Starting with AutoCAD 2021 -based products, FAS files can be compiled into two different file formats; Unicode and Multi-byte Character Strings (MBSCs). Unicode format FAS files are not compatible with AutoCAD 2020 -based and earlier product releases, but are required to properly support Unicode strings. Use the LISPSYS system variable to control the format in which to compile FAS files.

## Release Information

- AutoCAD R14 and later on Windows

## History

- Updated to support the Unicode file format.

## Examples

Assuming that *yinyang.lsp*  resides in a directory that is in the AutoCAD support file search path, the following command compiles this program:

```lisp
(vlisp-compile 'st "yinyang.lsp")

T
```

The output file is named *yinyang.fas*  and resides in the same directory as the source file.

The following command compiles *yinyang.lsp*  and names the output file *GoodKarma.fas*:

```lisp
(vlisp-compile 'st "yinyang.lsp" "GoodKarma.fas")
```

Note that the output file from the previous command resides in the AutoCAD installation directory, *not*  the directory where *yinyang.lsp*  resides. The following command compiles *yinyang.lsp*  and directs the output file to the *c:\my documents*  directory:

```lisp
(vlisp-compile 'st
"
yinyang.lsp
"
 "c:/my documents/GoodKarma")
```

This last example identifies the full path of the file to be compiled:

```lisp
(vlisp-compile 'st "<AutoCAD installation directory>/Sample/yinyang.lsp")
```

The output file from this command is named *yinyang.fas*  and resides in the same directory as the input file.
