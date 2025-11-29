---
title: initdia (AutoLISP)
guid: "GUID-D0F621C5-6D32-4D79-BC49-290CC4D0FDB8"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D0F621C5-6D32-4D79-BC49-290CC4D0FDB8.htm"
generated: "2025-11-28T19:06:33.031669Z"
description: Forces the display of the next command's dialog box
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

# initdia (AutoLISP)

> Forces the display of the next command's dialog box

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D0F621C5-6D32-4D79-BC49-290CC4D0FDB8.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-D0F621C5-6D32-4D79-BC49-290CC4D0FDB8.htm)
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
(initdia
[dialogflag]
)
```

- ***dialogflag*:** **Type:**  Integer  A numeric value. If this argument is not present or is present and nonzero, the next use (and next use only) of a command will display that command's dialog box rather than its command line prompts.  If *dialogflag*  is zero, any previous call to this function is cleared, restoring the default behavior of presenting the command line interface.

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

Currently, the following commands make use of the `initdia`  function: ATTDEF, ATTEXT, BHATCH, BLOCK, COLOR, IMAGE, IMAGEADJUST, INSERT, LAYER, LINETYPE, MTEXT, PLOT, RENAME, STYLE, TOOLBAR, and VIEW.

## Examples

Issue the PLOT command without calling `initdia`  first:

Command: **(command "._plot")**

plot

Enter a layout name <Model>: nil

Enter a layout name <Model>:

AutoCAD prompts for user input in the command window.

Use the following sequence of function calls to make AutoCAD display the Plot dialog box:

```lisp
(initdia)
(command "._plot")
```
