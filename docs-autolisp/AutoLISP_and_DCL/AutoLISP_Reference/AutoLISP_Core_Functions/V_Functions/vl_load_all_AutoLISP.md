---
title: "vl-load-all (AutoLISP)"
guid: "GUID-116F4D02-4B1A-4298-B38C-5B38FD982867"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-116F4D02-4B1A-4298-B38C-5B38FD982867.htm"
generated: "2025-11-28T19:06:48.271586Z"
description: Loads a file into all open AutoCAD documents, and into any document subsequently opened during the current AutoCAD session
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

# vl-load-all (AutoLISP)

> Loads a file into all open AutoCAD documents, and into any document subsequently opened during the current AutoCAD session

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-116F4D02-4B1A-4298-B38C-5B38FD982867.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-116F4D02-4B1A-4298-B38C-5B38FD982867.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Syntax

```lisp
(vl-load-all
filename
)
```

- ***filename*:** **Type:**  String  File to be loaded. If the file is in the AutoCAD support file search path, you can omit the path name, but you must always specify the file extension; `vl-load-all`  does not assume a file type.

## Return Values

**Type:**  nil or error

`nil`  if successful; otherwise, an error occurs when *filename*  is not found.

## Examples

- **Windows:** **(vl-load-all "c:/my documents/visual lisp/examples/whichns.lsp")**  nil **(vl-load-all "yinyang.lsp")**  nil
- **Mac OS:** **(vl-load-all "/myutilities/lsp/utilities.lsp")**  nil **(vl-load-all "utilities.lsp")**  nil
