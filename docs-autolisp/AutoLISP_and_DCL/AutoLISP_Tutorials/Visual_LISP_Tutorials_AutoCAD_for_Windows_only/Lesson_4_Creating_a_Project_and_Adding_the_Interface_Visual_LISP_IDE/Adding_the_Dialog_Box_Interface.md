---
title: Adding the Dialog Box Interface
guid: "GUID-7FB6239E-43D2-4000-A26B-5CDD1697AA1D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-7FB6239E-43D2-4000-A26B-5CDD1697AA1D.htm"
generated: "2025-11-28T19:07:00.066806Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Adding the Dialog Box Interface

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-7FB6239E-43D2-4000-A26B-5CDD1697AA1D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-7FB6239E-43D2-4000-A26B-5CDD1697AA1D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The next part of this lesson concerns adding a dialog box interface to the garden path application. To do this, you will be working with another language, *dialog control language (DCL)*.

Currently, your `gpath`  function only accepts input on the command line. You included a stubbed-out function `(gp:getDialogInput)`  with the intention of adding a dialog box interface. Now is the time to add the interface.

There are two steps in creating a functional dialog interface:

- Define the appearance and contents of the dialog boxes.
- Add program code to control dialog behavior.

The description and format of a dialog box is defined in a . *dcl*  file.

Program code that initializes default settings and responds to user interaction will be added to `gp:getDialogInput`.
