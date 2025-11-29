---
title: grtext (AutoLISP)
guid: "GUID-5B0B01CA-BDBA-4A77-B840-6EE64591AC47"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5B0B01CA-BDBA-4A77-B840-6EE64591AC47.htm"
generated: "2025-11-28T19:06:32.347836Z"
description: Writes text to the status line or to screen menu areas
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

# grtext (AutoLISP)

> Writes text to the status line or to screen menu areas

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5B0B01CA-BDBA-4A77-B840-6EE64591AC47.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5B0B01CA-BDBA-4A77-B840-6EE64591AC47.htm)
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
(grtext
[box text [highlight]]
)
```

- ***box*:** **Type:**  Integer  Location in which to write the text.
- ***text*:** **Type:**  String  Specifies the text to be written to the screen menu or status line location. The *text*  argument is truncated if it is too long to fit in the available area.
- ***highlight*:** **Type:**  Integer  Selects or deselects a screen menu location.

## Return Values

**Type:**  String

The string passed in the *text*  argument, if successful, and `nil`  if unsuccessful or no arguments are supplied.

## Remarks

This function displays the supplied text in the menu area; it does not change the underlying menu item. The `grtext`  function can be called with no arguments to restore all text areas to their standard values.

Note:
 This function is supported on Mac OS and Web, but does not affect the program.

If called without arguments, `grtext` restores all text areas to their standard values. If called with only one argument, `grtext`  results in an error.

- **Screen Menu Area (Obsolete):** Setting *box*  to a positive or zero value specifies a screen menu location. Valid *box*  values range from 0 to the highest-numbered screen menu box minus 1. The AutoCAD SCREENBOXES system variable reports the maximum number of screen menu boxes. If the *highlight*  argument is supplied as a positive integer, `grtext`  highlights the text in the designated box. Highlighting a box automatically dehighlights any other box already highlighted. If *highlight*  is zero, the menu item is dehighlighted. If *highlight*  is a negative number, it is ignored. On some platforms, the text must first be written without the *highlight*  argument and then must be highlighted. Highlighting of a screen menu location works only when the cursor is not in that area.
- **Status Line Area:** If `grtext`  is called with a *box*  value of -1, it writes the text into the mode status line area. The length of the mode status line differs from display to display (most allow at least 40 characters). The following code uses the `$(linelen)`  DIESEL expression to report the length of the mode status area.  (setq modelen (menucmd "M=$(linelen)"))  If a *box*  value of -2 is used, `grtext`  writes the text into the coordinate status line area. If coordinate tracking is turned on, values written into this field are overwritten as soon as the pointer sends another set of coordinates. For both -1 and -2 *box*  values, the *highlight*  argument is ignored.

## Examples

N/A
