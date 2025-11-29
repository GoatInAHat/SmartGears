---
title: AutoLISP and Visual LISP (AutoLISP)
guid: "GUID-49AAEA0E-C422-48C4-87F0-52FCA491BF2C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-49AAEA0E-C422-48C4-87F0-52FCA491BF2C.htm"
generated: "2025-11-28T19:05:57.793491Z"
description: AutoLISP is a programming language designed for extending and customizing AutoCAD product functionality.
topic_type: "concept-adsk"
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

# AutoLISP and Visual LISP (AutoLISP)

> AutoLISP is a programming language designed for extending and customizing AutoCAD product functionality.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-49AAEA0E-C422-48C4-87F0-52FCA491BF2C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-49AAEA0E-C422-48C4-87F0-52FCA491BF2C.htm)
- Topic Type: concept-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

It is based on the LISP programming language, whose origins date back to the late 1950s. LISP was originally designed for use in Artificial Intelligence (AI) applications, and is still the basis for many AI applications.

Autodesk introduced AutoLISP as an application programming interface (API) in Release 2.1, in the mid-1980s. LISP was chosen as the initial AutoCAD API because it was uniquely suited for the unstructured design process of AutoCAD projects, which involved repeatedly trying different solutions to design problems.

AutoLISP programs can be developed using a

- Basic ASCII text editor, such as Notepad on Windows or TextEdit on Mac OS
- Specialized editor, such as the Visual LISP integrated development environment (IDE) in AutoCAD for Windows or Microsoft Visual Studio (VS) Code with the AutoLISP Extension installed on Windows or Mac OS
  Note:
   The Visual LISP integrated development environment (IDE) is not available in AutoCAD LT for Windows or on Mac OS.

Specialized editors, such as the VL IDE and VS Code, provide tools to assist in the tasks of source-code creation and modification, program testing, and debugging.

After an AutoLISP program is written, it must loaded into the product before it can be used or debugged. Debugging your program allows you to evaluate and verify the code is working as expected, and if not identify what might be going wrong. The basics of debugging involve adding statements to the code and reviewing the contents of variables at strategic points in the program. If you discover you still do not have enough information to determine the error, you change the code again by adding additional debugging points. And finally, when you get the program working, you can either comment out or remove the debugging code.
