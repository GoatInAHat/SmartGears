---
title: "vl-file-delete (AutoLISP)"
guid: "GUID-4CEDC718-EEBB-48A8-A9D3-387F216C12B8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CEDC718-EEBB-48A8-A9D3-387F216C12B8.htm"
generated: "2025-11-28T19:06:46.825586Z"
description: Deletes a file
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

# vl-file-delete (AutoLISP)

> Deletes a file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CEDC718-EEBB-48A8-A9D3-387F216C12B8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CEDC718-EEBB-48A8-A9D3-387F216C12B8.htm)
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
(vl-file-delete
filename
)
```

- ***filename*:** **Type:**  String  Name of the file to be deleted. If you do not specify a full path name, `vl-file-delete`  searches the AutoCAD default drawing directory.

## Return Values

**Type:**  T or nil

`T`  if successful; `nil`  if delete failed.

## Release Information

- AutoCAD R14 and later on Windows
- AutoCAD 2011 and later on Mac OS

## History

- filename
   argument previously accepted an ASCII text string, but now accepts a Unicode text string.
- LISPSYS system variable controls which AutoLISP engine is used and the behavior of the function.
  - 0 - ASCII character support (legacy behavior)
  - 1 or 2 - Unicode character support

  Note:
   After the value of the LISPSYS system variable has been changed, AutoCAD must be restarted for the change to take affect.

## Examples

- **Windows:** Delete *newauto.bat*:  **(vl-file-delete "newauto.bat")**  nil  Nothing was deleted because there is no *newauto.bat*  file in the AutoCAD default drawing directory.  Delete the *newauto.bat*  file in the *c:\*  directory:  **(vl-file-delete "c:/newauto.bat")**  T  The delete was successful because the full path name identified an existing file.
- **Mac OS:** Delete *newstart.sh*:  **(vl-file-delete "newstart.sh")**  nil  Nothing was deleted because there is no *newstart.sh*  file in the AutoCAD default drawing directory.  Delete the *newstart.sh*  file in the / <root> directory:  **(vl-file-delete "/newstart.sh")**  T  The delete was successful because the full path name identified an existing file.
