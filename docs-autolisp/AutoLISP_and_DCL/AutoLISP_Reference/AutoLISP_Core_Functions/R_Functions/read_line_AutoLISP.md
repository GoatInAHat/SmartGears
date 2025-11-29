---
title: "read-line (AutoLISP)"
guid: "GUID-AC74D827-0969-4888-91C0-C3149DEC3659"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AC74D827-0969-4888-91C0-C3149DEC3659.htm"
generated: "2025-11-28T19:06:39.456569Z"
description: "Reads a string from the keyboard or from an open file, until an end-of-line marker is encountered"
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

# read-line (AutoLISP)

> Reads a string from the keyboard or from an open file, until an end-of-line marker is encountered

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AC74D827-0969-4888-91C0-C3149DEC3659.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AC74D827-0969-4888-91C0-C3149DEC3659.htm)
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
(read-line
[file-desc]
)
```

- ***file-desc*:** **Type:**  File  A file descriptor (obtained from `open`) referring to an open file. If no *file-desc*  is specified, `read-line`  obtains input from the keyboard input buffer.

## Return Values

**Type:**  String

The text read by `read-line`, without the end-of-line marker. If `read-line`  encounters the end of the file, it returns `nil`.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- Return value was modified to support Unicode characters and might be different than earlier releases.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  -  1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

- **Windows:** Open a file for reading:  **(setq f (open "c:\\my documents\\new.tst" "r"))**  #<file "c:\\my documents\\new.tst">  Use `**read-line**`  to read a line from the file:  **(read-line f)**  "To boldly go where nomad has gone before."  Obtain a line of input from the user:  **(read-line)**  **To boldly go**  "To boldly go"
- **Mac OS and Web:** Open a file for reading:  **(setq f (open "/my documents/new.tst" "r"))**  #<file "/my documents/new.tst">  Command:  Use `read-line`  to read a line from the file:  **(read-line f)**  "To boldly go where nomad has gone before."  Obtain a line of input from the user:  **(read-line)**  **To boldly go**  "To boldly go"
