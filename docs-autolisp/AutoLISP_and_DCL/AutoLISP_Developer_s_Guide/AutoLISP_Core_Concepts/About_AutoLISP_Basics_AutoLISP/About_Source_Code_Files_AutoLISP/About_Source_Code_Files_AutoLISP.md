---
title: About Source Code Files (AutoLISP)
guid: "GUID-299DBF3D-7896-465A-9F79-D654E7E48F25"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-299DBF3D-7896-465A-9F79-D654E7E48F25.htm"
generated: "2025-11-28T19:06:02.013510Z"
description: Although you can enter AutoLISP code at the AutoCAD Command prompt or Visual LISP Console window prompt (AutoCAD for Windows only), any functions you define are lost when you close the drawing or session they were created in.
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

# About Source Code Files (AutoLISP)

> Although you can enter AutoLISP code at the AutoCAD Command prompt or Visual LISP Console window prompt (AutoCAD for Windows only), any functions you define are lost when you close the drawing or session they were created in.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-299DBF3D-7896-465A-9F79-D654E7E48F25.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-299DBF3D-7896-465A-9F79-D654E7E48F25.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

Note:
 The Visual LISP IDE is not available in AutoCAD LT for Windows and on Mac OS.

AutoLISP source code can be saved to an ASCII text file with a *.lsp*  extension. Saving AutoLISP source code to a file has the following advantages:

- Programs are not lost when the drawing they are defined in is closed.
- Programs can be used in more than one drawing and can be executed on multiple machines.
- Programs can be shared with others in your organization.
- Testing and debugging multiple expressions is considerably easier.

AutoLISP source code can also be stored in files with a *.mnl*  extension. A Menu AutoLISP (MNL) file contains custom functions and commands that are required for the elements defined in a customization (CUIx) file. A MNL file is loaded automatically when it has the same name as a customization (CUIx) file that is loaded into the AutoCAD-based product.

Note:
 AutoCAD LT doesn't support the automatic loading of MNL files, but the files can be loaded using the AutoLISP
LOAD
 function from another LISP file.

For example, in AutoCAD for Windows, when *acad.cuix*  is loaded, the file named *acad.mnl*  is also loaded if it is found in one of the folders listed as part of the program's Support File Search Path. If a CUIx file does not have a corresponding MNL file, no error is displayed, the product just moves and loading other support files.

Note:
 While AutoLISP source code is commonly saved in files with a
.lsp
 or
.mnl
 extension, AutoLISP code can be loaded from any ASCII text file.
