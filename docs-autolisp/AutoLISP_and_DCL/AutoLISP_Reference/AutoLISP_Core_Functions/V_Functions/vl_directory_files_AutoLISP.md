---
title: "vl-directory-files (AutoLISP)"
guid: "GUID-C28C0CB0-FBBE-4AA8-BAC4-2FF222772514"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C28C0CB0-FBBE-4AA8-BAC4-2FF222772514.htm"
generated: "2025-11-28T19:06:45.962079Z"
description: Lists all files in a given directory
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

# vl-directory-files (AutoLISP)

> Lists all files in a given directory

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C28C0CB0-FBBE-4AA8-BAC4-2FF222772514.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C28C0CB0-FBBE-4AA8-BAC4-2FF222772514.htm)
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
(vl-directory-files
[directory pattern directories]
)
```

- ***directory*:** **Type:**  String  Directory name to collect files for; if `nil`  or absent, `vl-directory-files`  uses the current directory.
- ***pattern*:** **Type:**  String  Valid wildcard pattern for the file name; if `nil`  or absent, `vl-directory-files`  assumes "*.*".
- ***directories*:** **Type:**  Integer  Value that indicates whether the returned list should include directory names. Specify one of the following:  **-1**  -- List directories only  **0**  -- List files and directories (the default)  **1**  -- List files only

## Return Values

**Type:**  List or nil

A list of file and path names; otherwise `nil`  if no files match the specified pattern.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- directory
   and
  pattern
   arguments previously accepted ASCII text strings, but they now accept Unicode text strings.
- Return value was modified to support Unicode characters and might be different than earlier releases. For example,
  (vl-directory-files "c:/")
   previously returned the directory name "abc中" as "abc?", but now correctly returns the directory name as "abc中". Also previously passing a directory name with a Unicode character returned a value of
  nil
   even if the directory existed.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

- **Windows:** **(vl-directory-files "c:/acadwin" "acad*.exe")**  ("ACAD.EXE" "ACADAPP.EXE" "ACADL.EXE" "ACADPS.EXE") **(vl-directory-files "e:/acadwin" nil -1)**  ("." ".." "SUPPORT" "SAMPLE" "ADS" "FONTS" "IGESFONT" "SOURCE" "ASE") **(vl-directory-files "E:/acad" nil -1)**  ("." ".." "WIN" "COM" "DOS")
- **Mac OS:** **(vl-directory-files "/myutilities/lsp" "*.lsp")**  (".DS_Store" "utilities.lsp" "blk-insert.lsp") **(vl-directory-files "/myutilities" nil -1)**  ("." ".." "Help" "Lsp" "Support")
- **Web:** **(vl-directory-files "/acad/Support" "*.lin")**  ("acad.lin") **(vl-directory-files "/acad/Support" nil -1)**  ("." "..")
