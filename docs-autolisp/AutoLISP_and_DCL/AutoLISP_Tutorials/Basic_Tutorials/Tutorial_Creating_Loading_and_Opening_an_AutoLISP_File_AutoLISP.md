---
title: "Tutorial: Creating, Loading, and Opening an AutoLISP File (AutoLISP)"
guid: "GUID-3B8EDFF1-A130-434F-B615-7F2EC04322EE"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-3B8EDFF1-A130-434F-B615-7F2EC04322EE.htm"
generated: "2025-11-28T19:06:55.145117Z"
description: AutoLISP is an interpretive language, so it can be stored in a text file, loaded, and then executed directly within AutoCAD.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 25/04/2024
topic_subtype:
  - autolisp
  - tutorial
tags:
  - tutorial
  - create LSP
  - LSP file
  - load LSP
  - load AutoLISP file
  - load AutoLISP
  - create AutoLISP file
  - open LSP
  - open AutoLISP file
  - edit LSP file
  - edit AutoLISP file
  - edit AutoLISP
  - AutoLISP Notepad
  - AutoLISP TextEdit
  - Notepad
  - TextEdit
---

# Tutorial: Creating, Loading, and Opening an AutoLISP File (AutoLISP)

> AutoLISP is an interpretive language, so it can be stored in a text file, loaded, and then executed directly within AutoCAD.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-3B8EDFF1-A130-434F-B615-7F2EC04322EE.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-3B8EDFF1-A130-434F-B615-7F2EC04322EE.htm)
- Topic Type: concept
- Subtypes: autolisp, tutorial
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 25/04/2024
- Keywords: tutorial, create LSP, LSP file, load LSP, load AutoLISP file, load AutoLISP, create AutoLISP file, open LSP, open AutoLISP file, edit LSP file, edit AutoLISP file, edit AutoLISP, AutoLISP Notepad, AutoLISP TextEdit, Notepad, TextEdit

AutoLISP files typically have an *.lsp*  file extension, but they can also have the *.mnl*  file extension. Both LSP and MNL files can be edited with a text editor, such as Notepad on Windows and TextEdit on Mac OS.

Note:
 AutoLISP is not available in AutoCAD LT for Mac OS.

MNL files are associated with user interface customization and they are automatically loaded into AutoCAD when a customization (CUI/CUIx) file of the same name is loaded. For example, the *acad.mnl*  is automatically loaded into AutoCAD when the *acad.cuix*  file is loaded.

Note:
 AutoCAD LT doesn't support the automatic loading of MNL files, but the files can be loaded using the AutoLISP
LOAD
 function from another LISP file.

## Creating an AutoLISP (LSP) File

Here is how to create a file with the *.lsp*  extension and add the C:HELLO function defined in the *Creating a New Command and Working with System Variables*  tutorial.

- **Windows:** Click the Windows Start button, and then click Windows Accessories ![](../../../_assets/ac_menuaro-f6601f42.gif)  Notepad.  In Notepad, click File menu ![](../../../_assets/ac_menuaro-f6601f42.gif)  Save As.  In the Save As dialog box, browse to the *Documents*  folder. Right-click in an empty area, not over a file or folder, and click New ![](../../../_assets/ac_menuaro-f6601f42.gif)  Folder. Enter the name **LSP Files**  for the name of the new folder and press Enter. Double-click the new folder *LSP Files*  to make sure it is the current folder.  In the File Name text box, enter **Create-LSP-Tutorial.lsp**.  Click the Save As Type drop-down list and select All Files (*.*).  Click the Encoding drop-down list and select ANSI. Click Save.  In the editor area, enter the following (defun c:hello ( / msg) (setq msg (getstring T "\nEnter a message: ")) (alert msg) ) (prompt "\nAutoLISP Tutorial file loaded.") (princ) ; Suppress the return value of the prompt function  Click File menu ![](../../../_assets/ac_menuaro-f6601f42.gif)  Save.  Close Notepad.
- **Mac OS:** Note:  AutoLISP is not available in AutoCAD LT for Mac OS.  In Finder, on the Mac OS menu bar, click Go menu ![](../../../_assets/ac_menuaro-f6601f42.gif)  Applications.  In the Applications window, double-click TextEdit.  In TextEdit, on the Mac OS menu bar, click TextEdit menu ![](../../../_assets/ac_menuaro-f6601f42.gif)  Preferences.  In the Preferences dialog box, do the following: Under the Format section, click Plain Text.  Under the Options section, clear the Check Spelling As You Type and Correct Spelling Automatically check boxes.  Tip:  If needed, a rich text file can be switched to a plain text file by clicking Format menu ![](../../../_assets/ac_menuaro-f6601f42.gif)  Make Plain Text on the Mac OS menu bar.  Click the Close button.  On the Mac OS menu bar, click File menu ![](../../../_assets/ac_menuaro-f6601f42.gif)  New to create a new document.  With the new document current, on the Mac OS menu bar, click File menu ![](../../../_assets/ac_menuaro-f6601f42.gif)  Save.  In the Untitled dialog box, browse to the *Documents*  folder and click New Folder. Note:  The dialog box must be expanded to see the New Folder button.  In the New Folder dialog box, enter **LSP Files**  and click Create. Select the new folder *LSP Files*  to make sure it is the current folder.  In the Save As text box, enter **Create-LSP-Tutorial.lsp**.  If checked, clear the If No Extension is Provided, Use ".txt" check box.  Click Save.  In the editor area, enter the following (defun c:hello ( / msg) (setq msg (getstring T "\nEnter a message: ")) (alert msg) ) (prompt "\nAutoLISP Tutorial file loaded.") (princ) ; Suppress the return value of the prompt function  On the Mac OS menu bar, click File menu ![](../../../_assets/ac_menuaro-f6601f42.gif)  Save.  Quit TextEdit.

## Loading an AutoLISP (LSP) File

Here is how to load the *Create-LSP-Tutorial.lsp*  file created under the *Creating an AutoLISP (LSP) File*  section.

Note:
 AutoLISP is not available in AutoCAD LT for Mac OS.

1. In AutoCAD, do one of the following:
   - (Windows)
      On the ribbon, click Manage tab
      Applications panel
      Load Application.
   - (Mac OS)
      On the Mac OS menu bar, click Tools
      Load Application.
   - At the Command prompt, enter
     appload
     .
2. In the Load/Unload Applications dialog box, browse to the
   Documents

   LSP Files
    folder or the folder in which you stored the
   Create-LSP-Tutorial.lsp
    file.
3. Click Load.
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

   A message box displays the text string that you entered.

## Opening an AutoLISP (LSP) File

Here is how to open the *Create-LSP-Tutorial.lsp*  file that you created under the *Creating an AutoLISP (LSP) File*  section.

Do one of the following:

- (Windows)
   Double-click the
  Create-LSP-Tutorial.lsp
   file to open the file in Notepad.
- (Windows)
   Click the Windows Start button, and then click Windows Accessories
   Notepad. Click File menu
   Open. From the Save As Type drop-down list, select All Files (*.*). Browse to and select the
  Create-LSP-Tutorial.lsp
   file, and click Open.
- (Mac OS)
   Double-click the
  Create-LSP-Tutorial.lsp
   file to open the file in TextEdit.
  Note:
   If prompted for an application, click Choose Application. In the Choose Application dialog box, select TextEdit, and click Open.
- (Mac OS)
   In Finder, on the Mac OS menu bar, click Go menu
   Applications. In the Applications window, double-click TextEdit. In TextEdit, on the Mac OS menu bar, click File menu
   Open. Browse to and select the
  Create-LSP-Tutorial.lsp
   file, and click Open.
