---
title: "vl-file-copy (AutoLISP)"
guid: "GUID-12910252-217C-427D-8874-1323611FA74C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-12910252-217C-427D-8874-1323611FA74C.htm"
generated: "2025-11-28T19:06:46.718295Z"
description: Copies or appends the contents of one file to another file
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

# vl-file-copy (AutoLISP)

> Copies or appends the contents of one file to another file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-12910252-217C-427D-8874-1323611FA74C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-12910252-217C-427D-8874-1323611FA74C.htm)
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
(vl-file-copy
source-file destination-file [append]
)
```

- ***source-file*:** **Type:**  String  Name of file to be copied. If you do not specify a full path name, `vl-file-copy`  looks in the AutoCAD default drawing directory.
- ***destination-file*:** **Type:**  String  Name of the destination file. If you do not specify a path name, `vl-file-copy`  writes to the AutoCAD default drawing directory.
- ***append*:** **Type:**  T or nil  If specified and not `nil`, *source-file*  is appended to *destination-file*  (that is, copied to the end of the destination file).

## Return Values

**Type:**  Integer or nil

A numeric value, if the copy was successful; otherwise `nil`.

Some typical reasons for returning `nil`  are

- source-file
   is not readable
- source-file
   is a directory
- append?
   is absent or
  nil
   and
  destination-file
   exists
- destination-file
   cannot be opened for output (that is, it is an illegal file name or a write-protected file)
- source-file
   is the same as
  destination-file

## Remarks

Copy or append the contents of one file to another file. The `vl-file-copy`  function will not overwrite an existing file; it will only append to it.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- source-file
   and
  destination-file
   arguments previously accepted ASCII text strings, but they now accept Unicode text strings.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

- **Windows:** Copy *autoexec.bat*  to *newauto.bat*:  **(vl-file-copy "c:/autoexec.bat" "c:/newauto.bat")**  1417  Copy *test.bat*  to *newauto.bat*:  **(vl-file-copy "c:/test.bat" "c:/newauto.bat")**  nil  The copy fails because *newauto.bat*  already exists, and the *append*  argument was not specified.  Repeat the previous command, but specify *append*:  **(vl-file-copy "c:/test.bat" "c:/newauto.bat" T)**  185  The copy is successful because `T`  was specified for the *append*  argument.
- **Mac OS and Web:** Copy *oldstart.sh*  to *newstart.sh*:  **(vl-file-copy "/oldstart.sh" "/newstart.sh")**  1417  Copy *start.sh*  to *newstart.sh*:  **(vl-file-copy "/start.sh" "/newstart.sh")**  nil  The copy fails because *newstart.sh*  already exists, and the *append*  argument was not specified.  Repeat the previous command, but specify *append*:  **(vl-file-copy "/start.sh" "/newstart.sh" T)**  185  The copy is successful because `T`  was specified for the *append*  argument.
