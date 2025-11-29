---
title: "Tutorial: Garden Path (Visual LISP IDE)"
guid: "GUID-66377097-9BDB-4E3D-97E6-2263A5F273EC"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-66377097-9BDB-4E3D-97E6-2263A5F273EC.htm"
generated: "2025-11-28T19:06:56.924433Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
tags:
  - tutorial
  - tutorials
  - autolisp tutorial
  - examples
  - getting started
---

# Tutorial: Garden Path (Visual LISP IDE)

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-66377097-9BDB-4E3D-97E6-2263A5F273EC.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-66377097-9BDB-4E3D-97E6-2263A5F273EC.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 29/03/2023
- Keywords: tutorial, tutorials, autolisp tutorial, examples, getting started

Attention:
 This tutorial requires the Visual LISP Editor and applies to AutoCAD for Windows only.

## Overview

Your goal in this tutorial is to develop a new command for AutoCAD that draws a garden path and fills it with circular tiles. The tutorial is divided into seven lessons.

This tutorial covers

-  The Visual LISP
  ®
   environment is introduced. This environment provides you with editing, debugging, and other tools specific to the creation of AutoLISP applications.
- ActiveX
  ®
   and Reactor functions of AutoLISP are demonstrated, as well as several other extensions to the AutoLISP language provided with Visual LISP.

There are two possible execution contexts for this tutorial:

- The application may be run as interpreted LISP in piecemeal files and/or functions that are loaded into a single document, or
- The program code can be compiled into a VLX application, denoted by a
  *.vlx
   executable. A VLX operates from a self-contained namespace that can interact with the application-loading document.

## Lessons

Note:
 As you progress from lesson to lesson, you receive progressively less detailed instructions on how to perform individual tasks.

- **Basic:** [Lesson 1: Designing and Beginning the Program](Lesson_1_Designing_and_Beginning_the_Program_Visual_LISP_IDE.md)  [Lesson 2: Using Visual LISP Debugging Tools](Lesson_2_Using_Visual_LISP_Debugging_Tools_Visual_LISP_IDE.md)  [Lesson 3: Drawing the Path Boundary](Lesson_3_Drawing_the_Path_Boundary_Visual_LISP_IDE/Lesson_3_Drawing_the_Path_Boundary_Visual_LISP_IDE.md)
- **Intermediate:** [Lesson 4: Creating a Project and Adding the Interface](Lesson_4_Creating_a_Project_and_Adding_the_Interface_Visual_LISP_IDE/Lesson_4_Creating_a_Project_and_Adding_the_Interface_Visual_LISP_IDE.md)  [Lesson 5: Drawing the Tiles](Lesson_5_Drawing_the_Tiles_Visual_LISP_IDE/Lesson_5_Drawing_the_Tiles_Visual_LISP_IDE.md)
- **Advanced:** [Lesson 6: Acting With Reactors](Lesson_6_Acting_With_Reactors_Visual_LISP_IDE/Lesson_6_Acting_With_Reactors_Visual_LISP_IDE.md)  [Lesson 7: Putting It All Together](Lesson_7_Putting_It_All_Together_Visual_LISP_IDE/Lesson_7_Putting_It_All_Together_Visual_LISP_IDE.md)

## Assumptions

This tutorial assumes you have some familiarity with LISP or AutoLISP ®. It also assumes you understand basic Windows ®  file management tasks such as creating directories, copying files, and navigating through the file system on your hard disk or network.

## Sample Files

If you chose the full installation option when you installed AutoCAD, the source code files are in the following directory:

*<AutoCAD directory>\Tutorial\VisualLISP\*

It is recommended you do not modify the sample source code files supplied with AutoCAD. If something is not working correctly within your program, you may want to copy the supplied source code into your working directory. Throughout the tutorial, the working directory is referred to as:

*<AutoCAD directory>\Tutorial\VisualLISP\MyPath*

If you choose a different path for your working directory, substitute your directory name at the appropriate times.
