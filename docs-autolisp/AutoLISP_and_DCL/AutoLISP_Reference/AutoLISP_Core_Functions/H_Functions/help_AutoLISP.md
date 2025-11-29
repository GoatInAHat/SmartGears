---
title: help (AutoLISP)
guid: "GUID-4CB4A951-1507-4F3D-9F7B-93FF3A2C9850"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CB4A951-1507-4F3D-9F7B-93FF3A2C9850.htm"
generated: "2025-11-28T19:06:32.697447Z"
description: Invokes the Help facility
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

# help (AutoLISP)

> Invokes the Help facility

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CB4A951-1507-4F3D-9F7B-93FF3A2C9850.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4CB4A951-1507-4F3D-9F7B-93FF3A2C9850.htm)
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
(help
[helpfile [topic [command]]]
)
```

- ***helpfile*:** **Type:**  String  Help file name.  Note:  If an empty string is provided, AutoCAD will try to open the topic in the Online help associated with and available for the product.  The file extension is not required with the *helpfile*  argument. If a file extension is provided, AutoCAD looks only for a file with the exact name specified.  If no file extension is provided, AutoCAD looks for *helpfile*  with an extension of *.chm*. If no file of that name is found, AutoCAD looks for a file with an extension of *.hlp*.  Note:  It is recommended to place help files in a folder that doesn't contain Unicode characters. The Microsoft ®  HTML Help Executable doesn't support paths with Unicode characters, so the help files might not open as expected.
- ***topic*:** **Type:**  String  Help topic ID.  If you are calling a topic within a CHM file, provide the file name without the extension; AutoCAD adds an *.htm*  extension.
- ***command*  (Windows only):** **Type:**  String  Specifies the initial state of the Help window. The *command*  argument is a string used by the uCommand (in HTML Help) or the fuCommand (in WinHelp) argument of the HtmlHelp() and WinHelp() functions as defined in the Microsoft Windows SDK.  For HTML Help files, the *command*  parameter can be HH_ALINK_LOOKUP or HH_DISPLAY_TOPIC. For Windows Help files, the *command*  parameter can be HELP_CONTENTS, HELP_HELPONHELP, or HELP_PARTIALKEY.

## Return Values

**Type:**  String or nil

The *helpfile*  string, if successful; otherwise `nil`. If you use `help`  without any arguments, it returns an empty string (`""`) if successful, and `nil`  if it fails.

The only error condition that the `help`  function returns to the application is the existence of the file specified by *helpfile*. All other error conditions are reported to the user through a dialog box.

## Remarks

Note:
 This function is supported on Web, but does not affect the program.

## Examples

The following code calls `help`  to display the information on `MYCOMMAND`  in the Help file *achelp.chm*:

```lisp
(help "achelp.chm" "mycommand")
```
