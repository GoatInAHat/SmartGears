---
title: "read-char (AutoLISP)"
guid: "GUID-7E94BD14-F018-47D0-88DA-2B08DE32DB2C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E94BD14-F018-47D0-88DA-2B08DE32DB2C.htm"
generated: "2025-11-28T19:06:39.376580Z"
description: Returns the integer representing the character read from the keyboard input buffer or from an open file
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

# read-char (AutoLISP)

> Returns the integer representing the character read from the keyboard input buffer or from an open file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E94BD14-F018-47D0-88DA-2B08DE32DB2C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-7E94BD14-F018-47D0-88DA-2B08DE32DB2C.htm)
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
(read-char
[file-desc]
)
```

- ***file-desc*:** **Type:**  File  A file descriptor (obtained from `open`) referring to an open file. If no *file-desc*  is specified, `read-char`  obtains input from the keyboard input buffer.

## Return Values

**Type:**  Integer

Character code in the range of 1-65536.

The `read-char`  function returns a single newline character (code 10) whenever it detects an end-of-line character or character sequence.

## Release Information

- AutoCAD R12 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- Return value was modified to support Unicode characters and might be different than earlier releases. For example, suppose the "€" character was in a file named
  test.txt
  . Previously, the value of this character would be returned as ASCII code 128, but with Unicode support the value of this character would be returned as Unicode code 8364.

  ```lisp
  (setq fp (open "E:\\test.txt" "r" "utf8"))
  (read-char fp)
  8364
  ```
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  -  1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

The following example omits *file-desc*, so `read-char`  looks for data in the keyboard buffer:

```lisp
(read-char)
```

The keyboard buffer is empty, so `read-char`  waits for user input:

```lisp
ABC

65
```

The user entered `ABC`; `read-char`  returned the code representing the first character entered (A). The next three calls to `read-char`  return the data remaining in the keyboard input buffer. This data translates to 66 (the code for the letter B), 67 (C), and 10 (newline), respectively:

```lisp
(read-char)

66

(read-char)

67

(read-char)

10
```

With the keyboard input buffer now empty, `read-char`  waits for user input the next time it is called:

```lisp
(read-char)
```
