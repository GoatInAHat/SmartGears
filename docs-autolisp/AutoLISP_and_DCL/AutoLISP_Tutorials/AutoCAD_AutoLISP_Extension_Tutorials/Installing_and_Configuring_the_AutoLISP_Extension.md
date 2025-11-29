---
title: "Tutorial: Installing and Configuring the AutoLISP Extension (AutoLISP/VS Code)"
guid: "GUID-8EADDE55-CD92-422A-8493-9C7A19880629"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8EADDE55-CD92-422A-8493-9C7A19880629.htm"
generated: "2025-11-28T19:06:55.604869Z"
description: After you have installed Microsoft Visual Studio (VS) Code, you can install and configure the AutoCAD AutoLISP Extension which will allow you to manage and debug custom AutoLISP programs.
topic_type: concept
audience: programmer
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 29/03/2023
tags:
  - install autolisp extension
  - configure autolisp extension
---

# Tutorial: Installing and Configuring the AutoLISP Extension (AutoLISP/VS Code)

> After you have installed Microsoft Visual Studio (VS) Code, you can install and configure the AutoCAD AutoLISP Extension which will allow you to manage and debug custom AutoLISP programs.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8EADDE55-CD92-422A-8493-9C7A19880629.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-8EADDE55-CD92-422A-8493-9C7A19880629.htm)
- Topic Type: concept
- Audience: programmer
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 29/03/2023
- Keywords: install autolisp extension, configure autolisp extension

## Prerequisites

1. Install Visual Studio Code on your workstation.

## Topics in this Tutorial

- Install the AutoCAD AutoLISP Extension
- Add a Working Folder
- Configure the Debug Configurations (Not available for AutoCAD LT)

## Install the AutoCAD AutoLISP Extension

The AutoCAD AutoLISP Extension can be found in the Visual Studio Code Marketplace, accessible from inside of the Visual Studio Code application.

The following steps explain how to install the AutoCAD AutoLISP Extension from the Visual Studio Code Marketplace.

1. Launch Visual Studio Code.

   Do one of the following:

   - Windows:
      Click the Windows Start button, and then click Visual Studio Code > Visual Studio Code.
   - Mac OS:
      In Finder, click Go > Applications and then click Visual Studio Code in the Applications window.
2. In Visual Studio Code, on the Activity Bar, click Extensions (or click View menu > Extensions).
3. In the Extensions Search box, type
   autocad autolisp
   .
4.  From the search results list, click Install under the AutoCAD AutoLISP Extension item.

   After a few moments, the extension should be installed and listed in the Installed Extensions list.

## Add a Working Folder

A working folder is required to store configurations for debugging AutoLISP (LSP) files, but the folder can also be used to manage your LSP files.

The following steps explain how to create a folder named *LSP Files*  on your local drive in the *Documents*  folder and then how to open that folder in Visual Studio Code.

Note:
 Feel free to use a different name than
LSP Files
 or to store the folder in a different location other than
Documents
. Just remember to substitute the location and name throughout the remainder of the tutorials.

1. Do one of the following:
   - **Windows:** Click the Windows Start button, and then click Windows System > File Explorer or press Windows key + E.  In File Explorer, click in the address bar and type **Documents**. Press Enter to open the *Documents*  folder.  On the Home tab, click New folder from the New panel.  In the in-place editor, type **LSP Files**  and press Enter.
   - **Mac OS:** In Finder, on the Mac OS menu bar, click Go menu > Documents.  In the Documents window, on the Mac OS menu bar, click File menu > New Folder.  In the in-place editor, type **LSP Files**  and press Enter.
2. Switch to Visual Studio Code.
3. In Visual Studio Code, on the Activity Bar, click Explorer and then click Open Folder (or click File menu > Open Folder/Open).
4. In the Open Folder dialog box, browse to and select the
   LSP Files
    folder.
5. Click Select Folder on Windows or Open on Mac OS.

## Configure the Debug Configurations (Not supported with AutoCAD LT)

Visual Studio Code needs to be connected to AutoCAD for debugging, this connection is made through the use of a debug configuration.

Note:
 Debugging AutoLISP programs in AutoCAD LT isn't supported with the AutoLISP Extension for Microsoft Visual Studio (VS) Code.

The following steps explain how to add the necessary debug configurations needed to connect Visual Studio Code to AutoCAD.

1. In Visual Studio Code, click File menu > Preferences > Settings.
2. On the User tab, expand Extensions and click AutoCAD AutoLISP Configuration.
3. In the Debug: Attach Process text box, enter one of the following values in bold:
   - (Windows)
     acad
   - (Mac OS)
     AutoCAD

   Note:
    The process name is case sensitive, so acad or AutoCAD isn’t the same as ACAD or autocad.
4. In the Debug: Launch Program text box, enter the absolute path to the AutoCAD executable.

   The absolute path varies based on the release and platform on which AutoCAD was installed.

   - (Windows)
     "C:\Program Files\Autodesk\AutoCAD
     2025
     \acad.exe"
   - (Mac OS)
     "/Applications/Autodesk/AutoCAD
     2025
     /AutoCAD
     2025
     .app/Contents/MacOS/AutoCAD"
5. Optionally, in the Debug: Launch Parameter text box, specify any command line switches during the launch of the AutoCAD application.
