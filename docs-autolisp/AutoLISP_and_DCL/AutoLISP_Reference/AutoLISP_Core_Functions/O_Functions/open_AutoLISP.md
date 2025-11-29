---
title: open (AutoLISP)
guid: "GUID-089A323F-21FF-4337-99A9-375758E23BA4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-089A323F-21FF-4337-99A9-375758E23BA4.htm"
generated: "2025-11-28T19:06:38.155944Z"
description: Opens a file for access by the AutoLISP I/O functions
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

# open (AutoLISP)

> Opens a file for access by the AutoLISP I/O functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-089A323F-21FF-4337-99A9-375758E23BA4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-089A323F-21FF-4337-99A9-375758E23BA4.htm)
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
(open
filename mode [encoding]
)
```

- ***filename*:** **Type:**  String  Name and extension of the file to be opened. If you do not specify the full path name of the file, `open`  assumes you are referring to the AutoCAD default drawing directory.
- ***mode*:** **Type:**  String  Indicates whether the file is open for reading, writing, or appending. Specify a string containing one of the following letters:  **r**  Open for reading.  **w**  Open for writing. If *filename*  does not exist, a new file is created and opened. If *filename*  already exists, its existing data is overwritten. Data passed to an open file is not actually written until the file is closed with the `close`  function.  **a**  Open for appending. If *filename*  does not exist, a new file is created and opened. If *filename*  already exists, it is opened and the pointer is positioned at the end of the existing data, so new data you write to the file is appended to the existing data.  The *mode*  argument can be uppercase or lowercase.  Note:  Prior to AutoCAD 2000, *mode*  had to be specified in lowercase.
- ***encoding*:** **Type:**  String  Indicates the character encoding to use when reading the file. When a value isn't provided for the argument, the file is assumed to contain multibyte character set (MBCS) which is the legacy behavior.  Use one of the following values to specify a different character encoding:  **utf8**  UTF-8  **utf8-bom**  UTF-8 with Byte Order Marks

## Return Values

**Type:**  File or nil

If successful, `open`  returns a file descriptor that can be used by the other I/O functions. If mode `"r"`  is specified and *filename*  does not exist, `open`  returns `nil`.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- filename
   argument previously accepted an ASCII text string, but now accepts a Unicode text string.
- encoding
   argument is newly added.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support and no
    encoding
     argument (legacy behavior)
  -  1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

- mode
   argument previously only accepted a lowercase value.

## Examples

- **Windows:** Open an existing file:  **(setq a (open "c:/datafiles/filelist.txt" "r"))**  #<file "c:/datafiles/filelist.txt">  The following examples issue `open`  against files that do not exist:  **(setq f (open "c:\\my documents\\new.tst" "w"))**  #<file "c:\\my documents\\new.tst"> **(setq f (open "nosuch.fil" "r"))**  nil **(setq f (open "logfile" "a"))**  #<file "logfile">
- **Mac OS and Web:** Open an existing file:  **(setq a (open "/datafiles/filelist.txt" "r"))**  #<file "/datafiles/filelist.txt">  The following examples issue `open`  against files that do not exist:  **(setq f (open "/my documents/new.tst" "w"))**  #<file "/my documents/new.tst"> **(setq f (open "nosuch.fil" "r"))**  nil **(setq f (open "logfile" "a"))**  #<file "logfile">
