---
title: getint (AutoLISP)
guid: "GUID-18D6FF0F-B1DF-447D-BC6C-A46C933EF78B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-18D6FF0F-B1DF-447D-BC6C-A46C933EF78B.htm"
generated: "2025-11-28T19:06:31.026554Z"
description: Pauses for user input of an integer, and returns that integer
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

# getint (AutoLISP)

> Pauses for user input of an integer, and returns that integer

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-18D6FF0F-B1DF-447D-BC6C-A46C933EF78B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-18D6FF0F-B1DF-447D-BC6C-A46C933EF78B.htm)
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
(getint
[msg]
)
```

- ***msg*:** **Type:**  String  Message to be displayed to prompt the user; if omitted, no message is displayed.

## Return Values

**Type:**  Integer or nil

The integer specified by the user; otherwise `nil`, if the user presses Enter without entering an integer.

## Remarks

Values passed to `getint`  can range from -32,768 to +32,767. If the user enters something other than an integer, `getint`  displays the message, “Requires an integer value,” and allows the user to try again. The user cannot enter another AutoLISP expression as the response to a `getint`  request.

## Examples

```lisp
(setq num (getint))

15

15

(setq num (getint "Enter a number:"))

Enter a number:
25

25

(setq num (getint))

15.0

Requires an integer value.

15

15
```
