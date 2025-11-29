---
title: findtrustedfile (AutoLISP)
guid: "GUID-456F421A-DED1-4A87-9D4A-8434D1E997E1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-456F421A-DED1-4A87-9D4A-8434D1E997E1.htm"
generated: "2025-11-28T19:06:29.570077Z"
description: Searches the AutoCAD trusted locations for the specified file
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

# findtrustedfile (AutoLISP)

> Searches the AutoCAD trusted locations for the specified file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-456F421A-DED1-4A87-9D4A-8434D1E997E1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-456F421A-DED1-4A87-9D4A-8434D1E997E1.htm)
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
(findtrustedfile
filename
)
```

- ***filename*:** **Type:**  String  Name of the file to be searched for.

## Return Values

**Type:**  String or nil

A string containing the fully qualified file name; otherwise `nil`, if the specified file is not found.

## Remarks

The `findtrustedfile`  function makes no assumption about the file type or extension of *filename*.

## Examples

- **Windows:** If the trusted locations and support search file paths contains *C:\myapps*, and *abc.lsp*  is contained in the path, the function retrieves the path name:  **(findtrustedfile "abc.lsp")**  "C:\\myapps\\abc.lsp"  If the folder in which *abc.lsp*  is not present in both the trusted locations and support search file paths, `findtrustedfile`  returns `nil`.  **(findtrustedfile "abc.lsp")**  nil
- **Mac OS and Web:** If the trusted locations and support search file paths contains */myapps*, and *abc.lsp*  is contained in the path, the function retrieves the path name:  **(findtrustedfile "abc.lsp")**  "/myapps/abc.lsp"  If the folder in which *abc.lsp*  is not present in both the trusted locations and support search file paths, `findtrustedfile`  returns `nil`.  **(findtrustedfile "abc.lsp")**  nil
