---
title: Adding Reactor Callback Functions
guid: "GUID-D9080DD9-5C2D-4259-BB4E-04C0645B4510"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D9080DD9-5C2D-4259-BB4E-04C0645B4510.htm"
generated: "2025-11-28T19:07:04.384675Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 24/02/2021
topic_subtype:
  - autolisp
---

# Adding Reactor Callback Functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D9080DD9-5C2D-4259-BB4E-04C0645B4510.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D9080DD9-5C2D-4259-BB4E-04C0645B4510.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 24/02/2021

The reactor callback functions add a substantial amount of code to your application. This code is provided for you in the *Lesson6*  directory.

## To add the reactor callback functions to your program

1.  Copy the
   gpreact.lsp
    file from the
   Tutorial\VisualLISP\Lesson6
    directory to your
   MyPath
    working directory.
2.  Open the
   GPath
    project (if it is not already open), and click the Project Properties button in the gpath project window.
3. Add the
   gpreact.lsp
    file to your project.
4.  The
   gpreact.lsp
    file can reside anywhere in the order of files between
   utils.lsp
   , which must remain first, and
   gpmain.lsp
   , which should remain as the last file. Move any files, if necessary, then click OK.
5. Open the
   gpreact.lsp
    file by double-clicking on the file name within the gpath project window.

Read through the comments in the file to help you understand what it is doing. Note that all the callback functions are stubbed out; the only functionality they perform is to display alert messages when they are fired.

The last function in the file is so important it deserves a heading of its own.
