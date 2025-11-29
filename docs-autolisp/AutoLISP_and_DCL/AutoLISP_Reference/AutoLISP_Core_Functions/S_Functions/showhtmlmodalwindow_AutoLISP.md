---
title: showhtmlmodalwindow (AutoLISP)
guid: "GUID-1330BB1E-866E-419A-8AE3-22B0C16E1F06"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1330BB1E-866E-419A-8AE3-22B0C16E1F06.htm"
generated: "2025-11-28T19:06:41.210679Z"
description: Displays a modal dialog box with a specified URI (Uniform Resource Identifier)
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
  - function
---

# showhtmlmodalwindow (AutoLISP)

> Displays a modal dialog box with a specified URI (Uniform Resource Identifier)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1330BB1E-866E-419A-8AE3-22B0C16E1F06.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1330BB1E-866E-419A-8AE3-22B0C16E1F06.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 02/12/2019

**Supported Platforms:**  Windows only

## Signature

```lisp
(showhtmlmodalwindow
uri
)
```

- ***uri*:** **Type:**  String  URI of the page to be loaded into the dialog box.  The page that the URI references can utilize the AutoCAD JavaScript API.

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

Both the current size and position of the dialog box is automatically persisted between sessions.

## Examples

The following example displays the AutoCAD Developer Center.

```lisp
(showhtmlmodalwindow "https://www.autodesk.com/developautocad")
```
