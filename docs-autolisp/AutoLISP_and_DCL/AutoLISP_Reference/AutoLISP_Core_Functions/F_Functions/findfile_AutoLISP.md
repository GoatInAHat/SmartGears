---
title: findfile (AutoLISP)
guid: "GUID-D671F67D-F92B-41FF-B9FA-A48EF52CF607"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D671F67D-F92B-41FF-B9FA-A48EF52CF607.htm"
generated: "2025-11-28T19:06:29.482819Z"
description: Searches the AutoCAD library and trusted paths for the specified file or directory
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

# findfile (AutoLISP)

> Searches the AutoCAD library and trusted paths for the specified file or directory

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D671F67D-F92B-41FF-B9FA-A48EF52CF607.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D671F67D-F92B-41FF-B9FA-A48EF52CF607.htm)
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
(findfile
filename
)
```

- ***filename*:** **Type:**  String  Name of the file or directory to be searched for.

## Return Values

**Type:**  String or nil

A string containing the fully qualified file name; otherwise `nil`, if the specified file or directory is not found.

The file name returned by `findfile`  is suitable for use with the `open`  function.

## Remarks

The `findfile`  function makes no assumption about the file type or extension of *filename*. If *filename*  does not specify a drive/directory prefix, `findfile`  searches the AutoCAD library and trusted paths. If a drive/directory prefix is supplied, `findfile`  looks only in that directory.

## Examples

- **Windows:** If the current directory is */MyUtilities/lsp*  and it contains the file *abc.lsp*, the following function call retrieves the path name:  **(findfile "abc.lsp")**  "C:\\MyUtilities\\lsp\\abc.lsp"  If you are editing a drawing in the */My Utilities/Support*  directory, and the `ACAD`  environment variable is set to */My Utilities/Support*, and the file *xyz.txt*  exists only in the */My Utilities/Support*  directory, then the following command retrieves the path name:  **(findfile "xyz.txt")**  "C:\\My Utilities\\Support\\xyz.txt"  If the file *nosuch*  is not present in any of the directories on the library or trusted search paths, `findfile`  returns `nil`:  **(findfile "nosuch")**  nil
- **Mac OS and Web:** If the current directory is */MyUtilities/lsp*  and it contains the file *abc.lsp*, the following function call retrieves the path name:  **(findfile "abc.lsp")**  "/MyUtilities/Lsp/abc.lsp"  If you are editing a drawing in the */My Utilities/Support* directory, and the `ACAD`  environment variable is set to */My Utilities/Support*, and the file *xyz.txt*  exists only in the */My Utilities/Support*  directory, then the following command retrieves the path name:  **(findfile "xyz.txt")**  "/MyUtilities/Support/xyz.txt"  If the file *nosuch*  is not present in any of the directories on the library or trusted search paths, `findfile`  returns `nil`:  **(findfile "nosuch")**  nil
