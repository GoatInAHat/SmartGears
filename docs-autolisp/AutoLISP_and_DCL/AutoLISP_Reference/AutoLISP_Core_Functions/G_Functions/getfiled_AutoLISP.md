---
title: getfiled (AutoLISP)
guid: "GUID-AD65DF88-5218-4655-B877-B4D33B9FB6D1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AD65DF88-5218-4655-B877-B4D33B9FB6D1.htm"
generated: "2025-11-28T19:06:30.873623Z"
description: Prompts the user for a file name with the standard AutoCAD file dialog box, and returns that file name
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

# getfiled (AutoLISP)

> Prompts the user for a file name with the standard AutoCAD file dialog box, and returns that file name

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AD65DF88-5218-4655-B877-B4D33B9FB6D1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-AD65DF88-5218-4655-B877-B4D33B9FB6D1.htm)
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
(getfiled
title default ext flags
)
```

- ***title*:** **Type:**  String  The dialog box label.
- ***default*:** **Type:**  String  A default file name to use; can be a null string (`"")`.
- ***ext*:** **Type:**  String  The default file name extension. If *ext*  is passed as a null string (`"")`, it defaults to `*`  (all file types).  If the file type `dwg`  is included in the *ext*  argument, the `getfiled`  function displays an image preview in the dialog box.
- ***flags*:** **Type:**  Integer  An integer value (a bit-coded field) that controls the behavior of the dialog box. To set more than one condition at a time, add the values together to create a *flags*  value between 0 and 15. The following *flags*  arguments are recognized by `getfiled`:  **1**  (bit 0) -- Prompt for the name of a new file to create. Do not set this bit when you prompt for the name of an existing file to open. In the latter case, if the user enters the name of a file that doesn't exist, the dialog box displays an error message at the bottom of the box.  If this bit is set and the user chooses a file that already exists, AutoCAD displays an alert box and offers the choice of proceeding with or canceling the operation.  **4**  (bit 2) -- Let the user enter an arbitrary file name extension, or no extension at all.  If this bit is not set, `getfiled`  accepts only the extension specified in the *ext*  argument and appends this extension to the file name if the user doesn't enter it in the File text box.  **8**  (bit 3) -- If this bit is set and bit 0 is not set, `getfiled`  performs a library search for the file name entered. If it finds the file and its directory in the library search path, it strips the path and returns only the file name. (It does not strip the path name if it finds that a file of the same name is in a different directory.)  If this bit is not set, `getfiled`  returns the entire file name, including the path name.  Set this bit if you use the dialog box to open an existing file whose name you want to save in the drawing (or other database).  **16**  (bit 4) -- If this bit is set, or if the *default*  argument ends with a path delimiter, the argument is interpreted as a path name only. The `getfiled`  function assumes that there is no default file name. It displays the path in the Look in: line and leaves the File name box blank.  **32**  (bit 5) -- If this bit is set and bit 0 is set (indicating that a new file is being specified), users will not be warned if they are about to overwrite an existing file. The alert box to warn users that a file of the same name already exists will not be displayed; the old file will just be replaced.  **64**  (bit 6) -- Do not transfer the remote file if the user specifies a URL.  **128**  (bit 7) -- Do not allow URLs at all.

## Return Values

**Type:**  String or nil

If the dialog box obtains a file name from the user, `getfiled`  returns a string that specifies the file name; otherwise, it returns `nil`.

Note:
 This function is supported on Web, but always returns a value of 1.

## Remarks

The `getfiled`  function displays a dialog box containing a list of available files of a specified extension type. You can use this dialog box to browse through different drives and directories, select an existing file, or specify the name of a new file.

Note:
 This function is supported on Web, but does not allow the user to select files.

## Examples

The following call to `getfiled`  displays the Select a Lisp File dialog box:

- **Windows:** (getfiled "Select a Lisp File" "c:/program files/<AutoCAD installation directory>/support/" "lsp" 8)
- **Mac OS:** (getfiled "Select a Lisp File" "/Applications/Autodesk/<AutoCAD installation directory>/<product name>.app/" "lsp" 8)
