---
title: "Tutorial: Getting Started with the AutoLISP Extension (AutoLISP/VS Code)"
guid: "GUID-9999E8BF-CFA1-412F-A265-3568287DB77E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9999E8BF-CFA1-412F-A265-3568287DB77E.htm"
generated: "2025-11-28T19:06:55.732206Z"
description: The AutoCAD AutoLISP Extension for Microsoft Visual Studio (VS) Code allows you to write custom routines with the AutoLISP programming language that can be used to automate workflows and extend the functionality of AutoCAD.
topic_type: concept
audience: programmer
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 29/03/2023
tags:
  - create LSP file
  - autolisp extension
---

# Tutorial: Getting Started with the AutoLISP Extension (AutoLISP/VS Code)

> The AutoCAD AutoLISP Extension for Microsoft Visual Studio (VS) Code allows you to write custom routines with the AutoLISP programming language that can be used to automate workflows and extend the functionality of AutoCAD.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9999E8BF-CFA1-412F-A265-3568287DB77E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9999E8BF-CFA1-412F-A265-3568287DB77E.htm)
- Topic Type: concept
- Audience: programmer
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 29/03/2023
- Keywords: create LSP file, autolisp extension

## Prerequisites

1. Install Visual Studio Code on your workstation.
2. Install and configure the AutoCAD AutoLISP Extension.

## Topics in this Tutorial

- Create an AutoLISP (LSP) File
- Load an AutoLISP (LSP) File

## Launch Visual Studio (VS) Code

The following methods can be used to launch Visual Studio Code after it has been installed:

- In File Explorer, click Start menu > Visual Studio Code > Visual Studio Code.
- In AutoCAD, not AutoCAD LT, at the Command prompt, enter
  vlisp
  . If displayed, read and close the informational message to proceed.
  Note:
   Visual Studio Code might prompt for permission to allow the AutoCAD AutoLISP Extension to open a URI that displays a message box. Click Open to allow the extension to display the informational message during startup, or click Cancel to suppress the display of the informational message. Check "Don't ask again for this extension." to have VS Code remember your choice.
- In Finder, click Go > Applications and then click Visual Studio Code in the Applications window.

## Create an AutoLISP (LSP) File

LSP files are used to store AutoLISP code statements that can later be executed in a drawing and shared with others.

The following steps explain how to create an AutoLISP (LSP) file named *Create-LSP-Tutorial.lsp*  in the open working folder.

1. In Visual Studio Code, on the Activity Bar, click Explorer.
2. In the Explorer view, adjacent to the open folder's name, click New File.
3. In the in-place editor, type
   Create-LSP-Tutorial.lsp
    and press Enter.

   If you enter a different name, make sure to include the *.lsp*  file extension.
4. In the
   Create-LSP-Tutorial.lsp
    editor window, copy/paste or type the following:

   ```lisp
   ;; Displays a message box containing the entered text
   (defun c:hello ( / msg)
   (setq msg (getstring T "\nEnter a message: "))
   (alert msg)
   )

   ;; Draws a line between two points
   (defun c:drawline ( / pt1 pt2) ;; Declared local variables
   ;; Prompt for two points
   (setq pt1 (getpoint "\nSpecify start point of line: ")
   pt2 (getpoint pt1 "\nSpecify end point of line: ")
   )

   ;; Check to see if the user specified two points
   (if (and pt1 pt2)
   (command "_.line" pt1 pt2 "")
   (prompt "\nInvalid or missing point(s)")
   )

   ;; Exit quietly
   (princ)
   )

   (prompt "\nAutoLISP Tutorial file loaded.")
   (princ) ; Suppress the return value of the prompt function
   ```
5. One the menu bar, click File menu > Save to save the changes to the LSP file.
6. Click File menu > Close Editor or Close (X) on the file's tab to close the editor window.

## Load an AutoLISP (LSP) File

AutoLISP code statements stored in LSP files must be loaded into a drawing open in AutoCAD before they can be executed. While Visual Studio Code can be used to create and load LSP files, you will typically want to load your custom programs directly into AutoCAD before using them.

Note:
 You will need to load your LSP files into AutoCAD from Visual Studio Code when you want to debug the code statements that define your custom programs. Debugging the code statements stored in the
Create-LSP-Tutorial.lsp
 file is explained in
Tutorial: Debugging LSP Files with the AutoLISP Extension
.

The following steps explain how to load the *Create-LSP-Tutorial.lsp*  file into AutoCAD.

1. In AutoCAD, do one of the following:
   - (Windows)
      On the ribbon, click Manage tab > Applications panel > Load Application.
   - (Mac OS)
      On the Mac OS menu bar, click Tools > Load Application.
   - At the Command prompt, enter
     appload
     .
2. In the Load/Unload Applications dialog box, browse to the
   LSP Files
    folder or the folder in which you stored the
   Create-LSP-Tutorial.lsp
    file.
3. Select the
   Create-LSP-Tutorial.lsp
    file and click Load.
4. If the File Loading – Security Concern dialog box is displayed, click Load again.
5. Click Close to return to the application window.
6. You should see the following message in the Command History window.

   AutoLISP Tutorial file loaded.
7. At the Command prompt, enter
   hello
   .
8. At the
   Enter a message:
    prompt, type a text string and press Enter.

   A message box with the entered text string is displayed.
9. Click OK to close the message box.
