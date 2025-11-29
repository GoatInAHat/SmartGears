---
title: "acad-push-dbmod (AutoLISP)"
guid: "GUID-1B34A525-A881-42A8-BD07-865CA685F33B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1B34A525-A881-42A8-BD07-865CA685F33B.htm"
generated: "2025-11-28T19:06:21.447544Z"
description: Stores the current value of the DBMOD system variable
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

# acad-push-dbmod (AutoLISP)

> Stores the current value of the DBMOD system variable

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1B34A525-A881-42A8-BD07-865CA685F33B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-1B34A525-A881-42A8-BD07-865CA685F33B.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

**Prerequisites:**  The AcApp ObjectARX application must be loaded before the function can be called, which is loaded by default.

## Signature

```lisp
(acad-push-dbmod)
```

 No arguments.

## Return Values

**Type:**  T

Always returns `T`.

## Remarks

This function is used with `acad-pop-dbmod`  to control the `DBMOD`  system variable. You can use this function to change a drawing without changing the `DBMOD`  system variable. The `DBMOD`  system variable tracks changes to a drawing and triggers save-drawing queries.

This function pushes the current value of the `DBMOD`  system variable onto an internal stack. To use `acad-push-dbmod`  and `acad-pop-dbmod`, precede operations with `acad-push-dbmod`  and then use `acad-pop-dbmod`  to restore the original value of the `DBMOD`  system variable.

## Examples

The following example shows how to store the modification status of a drawing, change the status, and then restore the original status.

```lisp
(acad-push-dbmod)
(setq new_line '((0 . "LINE") (100 . "AcDbEntity") (8 . "0")
             (100 . "AcDbLine") (10 1.0 2.0 0.0) (11 2.0 1.0 0.0)
             (210 0.0 0.0 1.0)))
(entmake new_line)            ; Set DBMOD to flag 1
(command "._color" "2")        ; Set DBMOD to flag 4
(command "._-vports" "_SI")    ; Set DBMOD to flag 8
(command "._vpoint" "0,0,1")   ; Set DBMOD to flag 16
(acad-pop-dbmod)              ; Set DBMOD to original value
```
