---
title: About File Descriptors (AutoLISP)
guid: "GUID-EDADF856-4D64-4E19-A7BD-5017C4135A01"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EDADF856-4D64-4E19-A7BD-5017C4135A01.htm"
generated: "2025-11-28T19:06:01.662112Z"
description: A file descriptor is a pointer to a file opened by the AutoLISP open function.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# About File Descriptors (AutoLISP)

> A file descriptor is a pointer to a file opened by the AutoLISP open function.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EDADF856-4D64-4E19-A7BD-5017C4135A01.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EDADF856-4D64-4E19-A7BD-5017C4135A01.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

The `open`  function returns this pointer as an alphanumeric label. You supply the file descriptor as an argument to other AutoLISP functions that read, write, or close the file.

You use the following functions when working with a file:

- open
   - Opens a file for access by the AutoLISP I/O functions.
- write-line
   - Writes a string to the screen or to an open file.
- write-char
   - Writes one character to the screen or to an open file.
- read-line
   - Reads a string from the keyboard or from an open file, until an end-of-line marker is encountered.
- read-char
   - Returns the decimal ASCII code representing the character read from the keyboard input buffer or from an open file.
- print
   – Prints an expression to the command line or writes an expression to an open file.
- prin1
   – Prints an expression to the command line or writes an expression to an open file.
- close
   – Closes a file opened with the
  open
   function.

The following example opens the *myinfo.dat*  file for reading. The `open`  function returns a file descriptor which is stored in the file1 variable:

```lisp
(setq file1 (open "c:\\myinfo.dat" "r"))

#<file "c:\\myinfo.dat">
```

Files remain open until you explicitly close them in your AutoLISP program. The `close`  function closes a file. The following code closes the file whose file descriptor is stored in the file1 variable:

```lisp
(close file1)

nil
```
