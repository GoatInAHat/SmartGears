---
title: startapp (AutoLISP)
guid: "GUID-30EC493A-674B-4AAE-8C88-358E38CDAB21"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-30EC493A-674B-4AAE-8C88-358E38CDAB21.htm"
generated: "2025-11-28T19:06:42.671432Z"
description: Starts an external application
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

# startapp (AutoLISP)

> Starts an external application

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-30EC493A-674B-4AAE-8C88-358E38CDAB21.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-30EC493A-674B-4AAE-8C88-358E38CDAB21.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows and Mac OS only

## Signature

```lisp
(startapp
appcmd [file]
)
```

- ***appcmd*:** **Type:**  String  Application to execute. If *appcmd*  does not include a full path name, `startapp`  searches the directories in the PATH environment variable on Windows for the application and the equivalent on Mac OS.
- ***file*:** **Type:**  String  File name to be opened.

## Return Values

**Type:**  Integer or nil

A numeric value greater than 0, if successful; otherwise `nil`.

## Examples

- **Windows:** The following code starts Notepad and opens the *acad.lsp*  file.  **(startapp "notepad" "acad.lsp")**  33  If an argument has embedded spaces, it must be surrounded by literal double quotes. For example, to edit the file *my stuff.txt*  with Notepad, use the following syntax:  **(startapp "notepad.exe" "\"my stuff.txt\"")**  33
- **Mac OS:** The following code starts TextEdit and opens the *acad.lsp*  file.  **(startapp "textedit" "acad.lsp")**  33  If an argument has embedded spaces, it must be surrounded by literal double quotes. For example, to edit the file *my stuff.txt*  with TextEdit, use the following syntax:  **(startapp "textedit.app" "\"my stuff.txt\"")**  33
