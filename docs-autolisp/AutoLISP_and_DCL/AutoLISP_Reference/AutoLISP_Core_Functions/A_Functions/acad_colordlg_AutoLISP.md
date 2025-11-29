---
title: acad_colordlg (AutoLISP)
guid: "GUID-5E2A5569-02F4-496B-9CD7-E2457305FF35"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5E2A5569-02F4-496B-9CD7-E2457305FF35.htm"
generated: "2025-11-28T19:06:21.215230Z"
description: Displays the standard AutoCAD color selection dialog box
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

# acad_colordlg (AutoLISP)

> Displays the standard AutoCAD color selection dialog box

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5E2A5569-02F4-496B-9CD7-E2457305FF35.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5E2A5569-02F4-496B-9CD7-E2457305FF35.htm)
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
(acad_colordlg
colornum [flag]
)
```

- ***colornum*:** **Type:**  Integer  An integer in the range 0-256 (inclusive), specifying the AutoCAD color number to display as the initial default.  A *colornum*  value of 0 defaults to ByBlock, and a value of 256 defaults to ByLayer.
- ***flag*:** **Type:**  T or nil  If set to `nil`, disables the ByLayer and ByBlock buttons. Omitting the *flag*  argument or setting it to a non- `nil`  value enables the ByLayer and ByBlock buttons.

## Return Values

**Type:**  Integer or nil

The user-selected color number; otherwise `nil`, if the user cancels the dialog box.

## Examples

Prompt the user to select a color, and default to green if none is selected:

```lisp
(acad_colordlg 3)
```
