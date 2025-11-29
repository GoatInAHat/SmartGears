---
title: "write-char (AutoLISP)"
guid: "GUID-83AA4A55-C6A0-447B-A106-717E08D2EAF1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-83AA4A55-C6A0-447B-A106-717E08D2EAF1.htm"
generated: "2025-11-28T19:06:52.581421Z"
description: Writes one character to the screen or an open file
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

# write-char (AutoLISP)

> Writes one character to the screen or an open file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-83AA4A55-C6A0-447B-A106-717E08D2EAF1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-83AA4A55-C6A0-447B-A106-717E08D2EAF1.htm)
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
(write-char
num [file-desc]
)
```

- ***num*:** **Type:**  Integer  The integer value in the range of 1-65536 representing the character to be written.
- ***file-desc*:** **Type:**  File  A file descriptor for an open file.

## Return Values

**Type:**  Integer

The *num*  argument.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- num
   argument previously accepted an ASCII character code in the range of 1-255, but now accepts an integer that represents a Unicode character code in the range of 1-65536.
- Return value was modified to support Unicode characters and might be different than earlier releases. For example,
  (write-char 128)
   previously returned "€", but now returns "". If you want to return "€", your code will need to be updated to
  (write-char 8364)
  .

  ```lisp
  (setq fp (open "E:\\test.txt" "w" "utf8"))
  (write-char fp 8364)
  8364
  ```
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  -  1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

The following command writes the letter *C*  to the command window, and returns the supplied *num*  argument:

```lisp
(write-char 67)

C67
```

Assuming that `f`  is the descriptor for an open file, the following command writes the letter *C*  to that file:

```lisp
(write-char 67 f)

67
```

Note:

write-char
 cannot write a NULL character (code 0) to a file.
