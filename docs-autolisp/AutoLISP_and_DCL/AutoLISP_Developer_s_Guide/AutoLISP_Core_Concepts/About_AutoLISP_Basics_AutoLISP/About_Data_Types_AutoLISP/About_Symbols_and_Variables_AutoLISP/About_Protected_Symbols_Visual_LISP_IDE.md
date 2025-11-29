---
title: About Protected Symbols (Visual LISP IDE)
guid: "GUID-63D484B0-890D-438B-860D-A7202AE8B2A5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-63D484B0-890D-438B-860D-A7202AE8B2A5.htm"
generated: "2025-11-28T19:06:01.865027Z"
description: When Visual LISP is loaded, you may be warned if you attempt to change the value of some symbols used by the AutoLISP language.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# About Protected Symbols (Visual LISP IDE)

> When Visual LISP is loaded, you may be warned if you attempt to change the value of some symbols used by the AutoLISP language.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-63D484B0-890D-438B-860D-A7202AE8B2A5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-63D484B0-890D-438B-860D-A7202AE8B2A5.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

Note:
 The Visual LISP IDE is not available in AutoCAD LT for Windows and on Mac OS.

These symbols are known as protected symbols, and include items such as arithmetic operators (for example, **`+`**, **`-`**) and the values `T`  and `nil`.

You can use the Visual LISP Symbol Service feature to determine if a symbol is protected. When you first start AutoCAD, protected symbols receive no special protection. If you change a protected symbol at the AutoCAD Command prompt, no indication is made that a symbol has any special status. However, once you start Visual LISP, this changes. From the moment you start Visual LISP until the end of your AutoCAD session, Visual LISP intercepts any attempt to modify a protected symbol. Processing of protected symbols depends on the status of a Visual LISP environment option.

You can specify one of the following options:

- Transparent
   - Protected symbols are treated like any other symbol.
-  Print message
   - AutoLISP issues a warning message when you modify a protected symbol, but the modification is still carried out. For example, the following demonstrates what happens when you modify the predefined
  T
   variable:

  Command: **(setq t "look out")**

  ; *U* WARNING: assignment to protected symbol: T <- "look out"

  "look out"
-  Prompt to enter break loop
   - Results in Visual LISP displaying the following message box when you attempt to modify a protected symbol:

  Click Yes to interrupt processing and enter a Visual LISP break loop. Control switches to the Visual LISP Console window. To set the symbol and continue processing, click the Continue button on the Visual LISP toolbar; to abort modification, click Reset. If you click No, the symbol's value is modified, and processing continues normally.
- Error
   - This option prohibits modification of protected symbols. Any attempt to modify a protected symbol results in an error message.
