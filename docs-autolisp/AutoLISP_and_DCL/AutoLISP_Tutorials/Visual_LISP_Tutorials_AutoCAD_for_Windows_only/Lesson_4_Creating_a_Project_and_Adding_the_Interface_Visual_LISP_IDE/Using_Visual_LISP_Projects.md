---
title: Using Visual LISP Projects
guid: "GUID-83911E8D-D5A9-4FF0-A650-80466C5D1CFE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-83911E8D-D5A9-4FF0-A650-80466C5D1CFE.htm"
generated: "2025-11-28T19:06:59.982968Z"
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

# Using Visual LISP Projects

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-83911E8D-D5A9-4FF0-A650-80466C5D1CFE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-83911E8D-D5A9-4FF0-A650-80466C5D1CFE.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The Visual LISP project feature provides a convenient way to manage the files that make up your application. And with the project feature, you can open a single project file instead of individually opening every LISP file in the application. Once the project is open, getting to its constituent files is a double-click away.

## To create a Visual LISP project

1.  Click Project
    New Project from the Visual LISP menu.
2. Save the file in your
   Lesson4
    directory, using the name
   gpath.prj
   .

   After you save the file, Visual LISP displays the Project Properties dialog box.
3. Click the [Un]Select All button on the left in the Project Properties dialog box.
4.  Click the button containing an arrow pointing to the right. This adds all the selected files to your project.

   In the Project Properties dialog box, the list box on the left shows all LISP files that reside in the same directory as your project file and are *not*  included in that project. The list box on the right lists all the files that make up the project. When you add the selected files to the project, those file names move from the left box to the right box.
5.  In the list box on the right side of the dialog box, select
   gpmain
   , then click the Bottom button. This moves the file to the bottom of the list.

   Visual LISP loads project files in the order they are listed. Because the prompt that tells users the name of the command is located at the end of the *gpmain.lsp*  file, you need to move this file to the bottom of the list. Loading this file last results in the prompt displayed to users. The *utils.lsp*  file should be loaded first because it contains initialization code for the application. Therefore, select *utils*  in the dialog's list box and choose the Top button.
6.  Click OK.

Visual LISP adds a small project window to your Visual LISP desktop. The window lists the files in your project. Double-click on any file to open the file in the Visual LISP text editor (if it is not already open) and make it the active editor window.
