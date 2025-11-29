---
title: "vl-file-rename (AutoLISP)"
guid: "GUID-188B957C-4CDF-41C7-99BE-8080FA587F74"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-188B957C-4CDF-41C7-99BE-8080FA587F74.htm"
generated: "2025-11-28T19:06:46.997926Z"
description: Renames a file
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

# vl-file-rename (AutoLISP)

> Renames a file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-188B957C-4CDF-41C7-99BE-8080FA587F74.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-188B957C-4CDF-41C7-99BE-8080FA587F74.htm)
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
(vl-file-rename
old-filename new-filename
)
```

- ***old-filename*:** **Type:**  String  Name of the file you want to rename. If you do not specify a full path name, `vl-file-rename`  looks in the AutoCAD default drawing directory.
- ***new-filename*:** **Type:**  String  New name to be assigned to the file.  Note:  If you do not specify a path name, `vl-file-rename`  writes the renamed file to the AutoCAD default drawing directory.

## Return Values

**Type:**  T or nil

`T`, if renaming completed successfully; `nil`  if renaming failed.

Note:
 If the target file already exists, this function fails.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- old-filename
   and
  new-filename
   arguments previously accepted ASCII text strings, but they now accept Unicode text strings.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

- **Windows:** **(vl-file-rename "c:/newauto.bat" "c:/myauto.bat")**  T
- **Mac OS and Web:** **(vl-file-rename "/oldstartup.sh" "/mystartup.sh")**  T
