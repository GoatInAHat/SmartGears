---
title: alert (AutoLISP)
guid: "GUID-1BBC35EB-232F-4593-8B70-D9C473B7BC05"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1BBC35EB-232F-4593-8B70-D9C473B7BC05.htm"
generated: "2025-11-28T19:06:22.438999Z"
description: Displays a dialog box containing an error or warning message
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

# alert (AutoLISP)

> Displays a dialog box containing an error or warning message

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1BBC35EB-232F-4593-8B70-D9C473B7BC05.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1BBC35EB-232F-4593-8B70-D9C473B7BC05.htm)
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
(alert
msg
)
```

- ***msg*:** **Type:**  String  The message to display in the alert box.

## Return Values

**Type:**  nil

Always returns `nil`.

## Examples

Display a message in an alert box:

```lisp
(alert "That function is not available.")
```

Display a multiple line message, by using the newline character in *string*:

```lisp
(alert "That function\nis not available.")
```

Note:
 Line length and the number of lines in an alert box are platform, device, and window dependent. AutoCAD truncates any string that is too long to fit inside an alert box.
