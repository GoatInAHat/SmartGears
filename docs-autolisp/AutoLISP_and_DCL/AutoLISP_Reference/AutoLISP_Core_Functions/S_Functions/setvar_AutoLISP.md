---
title: setvar (AutoLISP)
guid: "GUID-B1CE1BFB-2448-47F1-95EC-10E9509F01FA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B1CE1BFB-2448-47F1-95EC-10E9509F01FA.htm"
generated: "2025-11-28T19:06:41.026955Z"
description: Sets an AutoCAD system variable to a specified value
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

# setvar (AutoLISP)

> Sets an AutoCAD system variable to a specified value

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B1CE1BFB-2448-47F1-95EC-10E9509F01FA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B1CE1BFB-2448-47F1-95EC-10E9509F01FA.htm)
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
(setvar
varname value
)
```

- ***varname*:** **Type:**  String  System variable name.  You can find a list of the current AutoCAD system variables in the product Help.
- ***value*:** **Type:**  Integer, Real, String, List, T, or nil  An atom or expression whose evaluated result is to be assigned to *varname*. For system variables with integer values, the supplied *value*  must be between -32,768 and +32,767.

## Return Values

**Type:**  Integer, Real, String, List, T, or nil

If successful, `setvar`  returns *value*.

## Remarks

Some AutoCAD commands obtain the values of system variables before issuing any prompts. If you use `setvar`  to set a new value while a command is in progress, the new value might not take effect until the next AutoCAD command.

When using the `setvar`  function to change the AutoCAD ANGBASE system variable, the *value*  argument is interpreted as radians. This differs from the AutoCAD SETVAR command, which interprets this argument as degrees. When using the `setvar`  function to change the AutoCAD system variable SNAPANG, the *value*  argument is interpreted as radians relative to the AutoCAD default direction for angle 0, which is *east*  or *3 o'clock*. This also differs from the SETVAR command, which interprets this argument as degrees relative to the ANGBASE setting.

Note:
 The UNDO command does not undo changes made to the AutoCAD CVPORT system variable by the
setvar
 function.

## Examples

Set the AutoCAD fillet radius to 0.5 units:

```lisp
(setvar "FILLETRAD" 0.50)

0.5
```
