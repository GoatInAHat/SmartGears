---
title: arxload (AutoLISP)
guid: "GUID-965A0D2A-CFD0-4D7C-9D2B-2D8188F0DAC8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-965A0D2A-CFD0-4D7C-9D2B-2D8188F0DAC8.htm"
generated: "2025-11-28T19:06:23.367541Z"
description: Loads an ObjectARX application
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 29/07/2024
topic_subtype:
  - autolisp
  - function
---

# arxload (AutoLISP)

> Loads an ObjectARX application

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-965A0D2A-CFD0-4D7C-9D2B-2D8188F0DAC8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-965A0D2A-CFD0-4D7C-9D2B-2D8188F0DAC8.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/07/2024

**Supported Platforms:**  Windows and Mac OS only

## Signature

```lisp
(arxload
application [onfailure]
)
```

- ***application*:** **Type:**  String  A quoted string or a variable that contains the name of an executable file. You can omit the extension from the file name; *.arx/.crx*  (Windows) or *.bundle*  (Mac OS).  You must supply the full path name of the ObjectARX executable file, unless the file is in a directory that is in the AutoCAD support file search path.
- ***onfailure*:** **Type:**  String  An expression to be executed if the load fails.

## Return Values

**Type:**  String

The application name, if successful. If unsuccessful and the *onfailure*  argument is supplied, `arxload`  returns the value of this argument; otherwise, failure results in an error message.

If you attempt to load an application that is already loaded, `arxload`  issues an error message. You may want to check the currently loaded ObjectARX applications with the `arx`  function before using `arxload`.

## Remarks

Important:
 Starting with AutoCAD 2014-based products, custom applications must work under secure mode; when the SECURELOAD system variable is set to 1 or 2. When operating under secure mode, the program is restricted to loading and executing files that contain code from trusted locations; trusted locations are specified by the TRUSTEDPATHS system variable.

## Examples

Load the *autoloader.arx*  file supplied in the AutoCAD installation directory:

```lisp
(arxload "
<AutoCAD installation directory>
/autoloader")

"
<AutoCAD installation directory>
/autoloader"
```
