---
title: Running the Application
guid: "GUID-5E37D54A-0A97-4200-ADCE-A6D2C2440960"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-5E37D54A-0A97-4200-ADCE-A6D2C2440960.htm"
generated: "2025-11-28T19:07:01.916254Z"
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

# Running the Application

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-5E37D54A-0A97-4200-ADCE-A6D2C2440960.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-5E37D54A-0A97-4200-ADCE-A6D2C2440960.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

Before running your program, save all the files you changed, if you have not already done so. You can click File ![](../../../../_assets/ac_menuaro-f6601f42.gif)  Save All from the Visual LISP menu, or use the Alt+Shift+S keyboard shortcut to save all your open files.

The next thing you must do is reload all the files in Visual LISP.

## To load and run all the files in your application

1.  If the project file you created earlier in this lesson is not already open, click Project
    Open Project from the Visual LISP menu, then enter the project file name
   gpath
   ; do not include the
   .prj
    extension. If Visual LISP does not find the project file, click the Browse button and choose the file from the Open Project dialog box. Click Open.
2. Click the Load Source Files button in the project window.
3. Enter
   (C:GPath)
    at the Visual LISP Console prompt to run the program. If you have some debugging to do, try using the tools you learned in Lessons 2 and 3. And remember, if all else fails, you can always copy the code from the
   Tutorial\VisualLISP\Lesson4
    directory.

Also, try drawing the path using both lightweight and old-style polylines. After drawing the paths, use the AutoCAD LIST command to determine whether or not your program is drawing the correct entity types.
