---
title: getstring (AutoLISP)
guid: "GUID-B139EFBD-74B7-4276-B422-D2186F7D8D0A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B139EFBD-74B7-4276-B422-D2186F7D8D0A.htm"
generated: "2025-11-28T19:06:31.636274Z"
description: Pauses for user input of a string, and returns that string
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

# getstring (AutoLISP)

> Pauses for user input of a string, and returns that string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B139EFBD-74B7-4276-B422-D2186F7D8D0A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-B139EFBD-74B7-4276-B422-D2186F7D8D0A.htm)
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
(getstring
[cr] [msg]
)
```

- ***cr*:** **Type:**  T or nil  If supplied and not `nil`, this argument indicates that users can include blanks in their input string (and must terminate the string by pressing Enter). Otherwise, the input string is terminated by entering a space or pressing Enter.
- ***msg*:** **Type:**  String  Message to be displayed to prompt the user.

## Return Values

**Type:**  String

The string entered by the user; otherwise `nil`, if the user pressed Enter without typing a string.

If the string is longer than 132 characters, `getstring`  returns only the first 132 characters of the string. If the input string contains the backslash character (\), `getstring`  converts it to two backslash characters (\\). This allows you to use returned values containing file name paths in other functions.

## Remarks

The user cannot enter another AutoLISP expression as the response to a `getstring`  request.

## Examples

```lisp
(setq s (getstring "What's your first name? "))

What's your first name?
Gary

"Gary"

(setq s (getstring T "What's your full name? "))

What's your full name?
Gary Indiana Jones

"Gary Indiana Jones"

(setq s (getstring T "Enter filename: "))

Enter filename:
c:\my documents\vlisp\secrets

"c:\\my documents\\vlisp\\secrets"
```
