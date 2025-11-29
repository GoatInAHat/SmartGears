---
title: "write-line (AutoLISP)"
guid: "GUID-CB4F3ABC-F0F6-41DA-A911-75B90D9F974A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CB4F3ABC-F0F6-41DA-A911-75B90D9F974A.htm"
generated: "2025-11-28T19:06:52.683540Z"
description: Writes a string to the screen or to an open file
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

# write-line (AutoLISP)

> Writes a string to the screen or to an open file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CB4F3ABC-F0F6-41DA-A911-75B90D9F974A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CB4F3ABC-F0F6-41DA-A911-75B90D9F974A.htm)
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
(write-line
str [file-desc]
)
```

- ***str*:** **Type:**  String  A textual value.
- ***file-desc*:** **Type:**  File  A file descriptor for an open file.

## Return Values

**Type:**  String

The *str*, quoted in the normal manner. The quotes are omitted when writing to a file.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- str
   argument previously accepted an ASCII text string or character, but now accepts a Unicode text string or character.
- Return value was modified to support Unicode characters and might be different than earlier releases.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  -  1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

- **Windows:** Open a file:  **(setq f (open "c:\\my documents\\new.tst" "w"))**  #<file "c:\\my documents\\new.tst">  Use `write-line`  to write a line to the file:  **(write-line "To boldly go where nomad has gone before." f)**  "To boldly go where nomad has gone before."  The line is not physically written until you close the file:  **(close f)**  nil
- **Mac OS:** Open a file:  **(setq f (open "/my documents/new.tst" "w"))**  #<file "/my documents/new.tst">  Use `write-line`  to write a line to the file:  **(write-line "To boldly go where nomad has gone before." f)**  "To boldly go where nomad has gone before."  The line is not physically written until you close the file:  **(close f)**  nil
