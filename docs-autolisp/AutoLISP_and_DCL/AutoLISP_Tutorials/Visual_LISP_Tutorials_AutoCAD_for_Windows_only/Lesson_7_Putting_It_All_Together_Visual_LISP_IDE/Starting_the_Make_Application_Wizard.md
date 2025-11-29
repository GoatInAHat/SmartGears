---
title: Starting the Make Application Wizard
guid: "GUID-D47DDBBE-E94D-4433-91AE-D31BF876CFB6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D47DDBBE-E94D-4433-91AE-D31BF876CFB6.htm"
generated: "2025-11-28T19:07:07.262467Z"
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

# Starting the Make Application Wizard

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D47DDBBE-E94D-4433-91AE-D31BF876CFB6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-D47DDBBE-E94D-4433-91AE-D31BF876CFB6.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

To assist you in creating standalone applications, Visual LISP provides the Make Application wizard.

## To run the Make Application wizard

1.  To start the wizard, click File
    Make Application
    New Application Wizard from the Visual LISP menu.
2. Select Expert mode and click Next.

   The wizard prompts you to specify the directory in which to store files created by Make Application, and to name your application. Make Application produces two output files: a . *vlx*  file containing your program executable, and a . *prv*  file containing the options you specify to Make Application. The . *prv*  file is also known as a *make*  file. You can use the make file to rebuild your application, when necessary.
3.  Specify your
   Tutorial\VisualLISP\MyPath
    directory as the application location, and call the application
   gardenpath
   . Visual LISP uses the application name in the output file names (in this instance,
   gardenpath.vlx
    and
   gardenpath.prv
   .)

   Click Next to continue.
4. The application options are not covered in this tutorial. Accept the defaults and click Next.
5. In this step, the wizard is prompting you to identify all the AutoLISP source code files that make up the application. You could individually select the LISP source files, but there is an easier way. Change the pull-down file type selection box so that “Visual LISP Project file” is shown, then click the Add button. Select the
   Gpath
    project file and click Open.
   Note:
    Depending on how you worked through the tutorial, you may have several
   Gpath
    project files showing up. Select the most recent file. If you copied in the completed source code from Lesson 7, the project name to select should be
   Gpath7.prj
   .

   After selecting the project file, click Next to continue.

   One advantage of compiled VLX applications is that you can compile your dialog control files (. *dcl*) into the complete application. This reduces the number of individual source files your end users need to deal with, and eliminates any of the search path problems when loading a DCL file.
6.  Change the pull-down file type selection box so that “DCL files” is shown, then click the Add button. Select the
   gpdialog.dcl
    file, then click Open.

   Click Next to continue building the application.
7. Compilation options are not covered in this tutorial. Accept the defaults and click Next.
8.  The final step is used to review the selections you've made. At this point, you can select Finish. Visual LISP will begin the build process, displaying the results in the Build Output window. Several intermediate files will be produced, as your individual source code files are compiled into a format that can be linked into the single VLX application.

When it is all complete, you'll have an executable file named *gardenpath.vlx*. To test it, do the following:

- From AutoCAD, click Manage tab
   Applications panel
   Load Application.
- Load the
  gardenpath.vlx
   application that was just created and is found in the
  Tutorial\VisualLISP\
  MyPath directory.
-  At the Command prompt, enter
  gpath
   and press Enter to execute the command from the VLX file.
