---
title: "Tutorial: Formatting LSP Files with the AutoLISP Extension (AutoLISP/VS Code)"
guid: "GUID-55FA8B38-1748-40D4-BFA6-A135335017DA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-55FA8B38-1748-40D4-BFA6-A135335017DA.htm"
generated: "2025-11-28T19:06:56.837216Z"
description: The AutoLISP Extension provides tools to format select AutoLISP code statements, define the indent and alignment of new statements as they are typed, and mark a statement as a comment.
topic_type: concept
audience: programmer
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 15/01/2021
tags:
  - format LSP files
  - autolisp extension
  - insert comment
  - format AutoLISP
  - indent AutoLISP
  - indent statements
  - toggle comment
---

# Tutorial: Formatting LSP Files with the AutoLISP Extension (AutoLISP/VS Code)

> The AutoLISP Extension provides tools to format select AutoLISP code statements, define the indent and alignment of new statements as they are typed, and mark a statement as a comment.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-55FA8B38-1748-40D4-BFA6-A135335017DA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-55FA8B38-1748-40D4-BFA6-A135335017DA.htm)
- Topic Type: concept
- Audience: programmer
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 15/01/2021
- Keywords: format LSP files, autolisp extension, insert comment, format AutoLISP, indent AutoLISP, indent statements, toggle comment

## Prerequisites

1. Install Visual Studio Code on your workstation.
2. Install and configure the AutoCAD AutoLISP Extension.
3. Open an AutoLISP (LSP) File for edit in Visual Studio Code.

## Topics in this Tutorial

- Format AutoLISP Code Statements
- Comment and Uncomment AutoLISP Code Statements

## Format AutoLISP Code Statements

Visual Studio Code automatically applies some formatting to new AutoLISP code statements as they are typed, but code statements pasted from another source are not automatically formatted. Visual Studio Code does allow you to format select or all code statements in the current editor window.

The following steps explain how to format code statements in an LSP file:

1. In Visual Studio Code, open the
   Create-LSP-Tutorial.lsp
    file or make it current.
2. In the editor window, select the following statements that define the
   hello
    function:

   ```lisp
   (defun c:hello ( / msg)
   (setq msg (getstring T "\nEnter a message: "))
   (alert msg)
   )
   ```
3. Right-click and choose Format Selection.

   The code statements are reformatted with the nested statements now indented and aligned. The statements should look like the following:

   ```lisp
   (defun c:hello (/ msg)
       (setq msg (getstring T "\nEnter a message: "))
       (alert msg)
   )
   ```
4. Right-click in the editor window and choose Format Document.

   All the code statements in the LSP file are formatted based on the settings of the AutoLISP Extension.
5. Save the changes to the
   Create-LSP-Tutorial.lsp
    file.

The current settings used to indent and format AutoLISP code statements can be viewed and changed by doing the following:

1. In Visual Studio Code, on the Activity Bar, click Extensions.
2. On the Extensions view, click More Actions > Show Installed Extensions.
3. On the AutoCAD AutoLISP Extension item, click Manage (
   ) > Extension Settings.
4. Adjust the extension settings as desired, changes are automatically saved.

   If you want to restore the default value of a setting, move the cursor over the name of the setting, and click More Actions (![](../../../_assets/GUID_51857ACF_72EA_416C_B411_A799B0F6CC41-2d6b8eb2.png)) > Reset Setting.

## Comment and Uncomment AutoLISP Code Statements

Comments are great for describing what a program or specific code statement does in an LSP file. You indicate a comment in an LSP file by adding one or more semi-colons in front of the text that you want to mark as a comment.

The following steps explain how to use the tools in Visual Studio Code to mark lines as comments or uncomment lines in an LSP file:

1. In Visual Studio Code, open the
   Create-LSP-Tutorial.lsp
    file or make it current.
2. In the editor window, select the following lines that define the
   hello
    function:

   ```lisp
   (defun c:hello (/ msg)
       (setq msg (getstring T "\nEnter a message: "))
       (alert msg)
   )
   ```
3. On the menu bar, click Edit menu > Toggle Line Comment.

   A semi-colon is added to the beginning of each line. The statements should now look like the following:

   ```lisp
   ; (defun c:hello (/ msg)
   ;     (setq msg (getstring T "\nEnter a message: "))
   ;     (alert msg)
   ; )
   ```
4. Select the lines again and click Edit menu > Toggle Line Comment.

   The semi-colon is removed from each line.

The Toggle Block Comment tool on the Edit menu can also be used to mark a large number of lines as comments. This type of comment is known as a *block comment*. A block comment starts with the character sequence ;| and ends with |; rather than each line beginning with a semi-colon. The following shows the results of a block comment being applied to the `hello`  function.

```lisp
;| (defun c:hello (/ msg)
    (setq msg (getstring T "\nEnter a message: "))
    (alert msg)
) |;
```

A comment can also be placed after a code statement, this is commonly known as an *inline comment*.

```lisp
(alert msg)  ; Displays a string in a message box
```

Tip:
 Comments can be helpful while debugging or making changes to a program. Marking code statements as comments allows you to suppress them from being executed in the AutoCAD program, while preserving them in the LSP file which allows you to narrow the focus of the code statements to debug and provide you with a copy of the existing code statements to reference.
