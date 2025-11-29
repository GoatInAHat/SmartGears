---
title: arxunload (AutoLISP)
guid: "GUID-29092087-D1F7-4DF5-863C-FAF4E4F1BC8B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-29092087-D1F7-4DF5-863C-FAF4E4F1BC8B.htm"
generated: "2025-11-28T19:06:23.442716Z"
description: Unloads an ObjectARX application
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

# arxunload (AutoLISP)

> Unloads an ObjectARX application

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-29092087-D1F7-4DF5-863C-FAF4E4F1BC8B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-29092087-D1F7-4DF5-863C-FAF4E4F1BC8B.htm)
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
(arxunload
application [onfailure]
)
```

- ***application*:** **Type:**  String  A quoted string or a variable that contains the name of a file that was loaded with the `arxload`  function. You can omit the extension from the file name; *.arx/.crx*  (Windows) or *.bundle*  (Mac OS).
- ***onfailure*:** **Type:**  String  An expression to be executed if the unload fails.

## Return Values

**Type:**  String

The application name, if successful. If unsuccessful and the *onfailure*  argument is supplied, `arxunload`  returns the value of this argument; otherwise, failure results in an error message.

Note that locked ObjectARX applications cannot be unloaded. ObjectARX applications are locked by default.

## Examples

Unload the Autoloader application that was loaded in the `arxload`  function example:

```lisp
(arxunload "autoloader")

"autoloader"
```
