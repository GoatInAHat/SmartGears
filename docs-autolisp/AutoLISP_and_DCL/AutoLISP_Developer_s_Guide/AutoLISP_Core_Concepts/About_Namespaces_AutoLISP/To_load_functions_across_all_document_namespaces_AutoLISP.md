---
title: To load functions across all document namespaces (AutoLISP)
guid: "GUID-08F778AF-6A57-41AA-8AF5-3E56FEB1958B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-08F778AF-6A57-41AA-8AF5-3E56FEB1958B.htm"
generated: "2025-11-28T19:05:59.124302Z"
description: "Normally, loading an application file loads it in the current drawing only, but the vl-load-all function can be used to load a file in all drawings."
topic_type: "task-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# To load functions across all document namespaces (AutoLISP)

> Normally, loading an application file loads it in the current drawing only, but the vl-load-all function can be used to load a file in all drawings.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-08F778AF-6A57-41AA-8AF5-3E56FEB1958B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-08F778AF-6A57-41AA-8AF5-3E56FEB1958B.htm)
- Topic Type: task-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

- At the AutoCAD Command prompt, enter an AutoLISP statement that uses the
  vl-load-all
   function and press Enter.

Note:
 The
vl-load-all
 function is useful for testing new functions in multiple documents, but in general you should use
acaddoc.lsp
 to load files that are needed in every document.

## Example

1. At the AutoCAD Command prompt, enter
   (load "yinyang.lsp")
    and press Enter.
2. Invoke a function defined in the AutoLISP source (LSP) file.

   The function should work as expected.
3. Create a new or open an existing drawing.
4. With the second drawing window active, try invoking the function again.

   The response will be an error message saying the function is not defined.
5. At the AutoCAD Command prompt, enter
   (vl-load-all "yinyang.lsp")
    and press Enter.
6. Create a new or open an existing drawing.
7. Invoke the function again.

   This time the function will work correctly because the `vl-load-all`  function loads the contents of an AutoLISP file into all open documents, and into any documents opened later in the session.
