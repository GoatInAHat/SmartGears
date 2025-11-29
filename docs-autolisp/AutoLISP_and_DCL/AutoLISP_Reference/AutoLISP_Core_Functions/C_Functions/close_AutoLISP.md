---
title: close (AutoLISP)
guid: "GUID-5308ADB6-7C46-43BE-8B6C-B32FBA8982DB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5308ADB6-7C46-43BE-8B6C-B32FBA8982DB.htm"
generated: "2025-11-28T19:06:25.548475Z"
description: Closes an open file
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

# close (AutoLISP)

> Closes an open file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5308ADB6-7C46-43BE-8B6C-B32FBA8982DB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-5308ADB6-7C46-43BE-8B6C-B32FBA8982DB.htm)
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
(close
file-desc
)
```

- ***file-desc*:** **Type:**  File  A file descriptor obtained from the `open`  function.

## Return Values

**Type:**  nil

`nil`  if *file-desc*  is valid; otherwise results in an error message.

After a `close`, the file descriptor is unchanged but is no longer valid. Data added to an open file is not actually written until the file is closed.

## Examples

The following code counts the number of lines in the file *somefile.txt*  and sets the variable *ct*  equal to that number:

```lisp
(setq fil "SOMEFILE.TXT")
(setq x (open fil "r") ct 0)
(while (read-line x)
  (setq ct (1+ ct))
)
(close x)
```
