---
title: About Redefining AutoCAD Commands (AutoLISP)
guid: "GUID-38D0244E-C0C7-4FF0-A4B9-DE6E05635BD6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-38D0244E-C0C7-4FF0-A4B9-DE6E05635BD6.htm"
generated: "2025-11-28T19:06:04.764732Z"
description: "Using AutoLISP, you can undefine and replace the functionality of a built-in AutoCAD command."
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# About Redefining AutoCAD Commands (AutoLISP)

> Using AutoLISP, you can undefine and replace the functionality of a built-in AutoCAD command.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-38D0244E-C0C7-4FF0-A4B9-DE6E05635BD6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-38D0244E-C0C7-4FF0-A4B9-DE6E05635BD6.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The UNDEFINE command allows you to disable a built-in AutoCAD command, and then using AutoLISP it can be replaced by defining a user-defined command of the same name with the `defun`  function. A command remains undefined for the current editing session only. The built-in definition of a command can be restored with the REDEFINE command.

You can execute the built-in definition of a command after it has been undefined by specifying its name prefixed with a period (.). For example, if you undefine QUIT, you can access the command by entering **.quit**  at the AutoCAD Command prompt. This is also the syntax that should be used within the AutoLISP command function to make sure your user-defined functions and commands work predictably even if a built-in command has been undefined.

It is recommended that you protect your menus, scripts, and AutoLISP programs by using the period-prefixed forms of all commands. This ensures that your applications use the built-in command definitions rather than a redefined command.

Consider the following example. Whenever you use the LINE command, you want AutoCAD to remind you about using the PLINE command. You can define the AutoLISP function `C:LINE`  to substitute for the normal LINE command as follows:

```lisp
(defun C:LINE ( )
  (princ "Shouldn't you be using PLINE?\n")
  (command ".LINE")
 (princ)
)

C:LINE
```

In this example, the function `C:LINE`  is designed to issue its message and then to execute the standard LINE command (using .LINE with the command function). Before AutoCAD can use your definition of the LINE command, you must undefine the built-in LINE command. Enter the following to undefine the built-in LINE command:

```lisp
(command ".undefine" "line")
```

After undefining the command and entering line at the AutoCAD Command prompt, AutoCAD uses the `C:LINE`  AutoLISP function:

Command: **line**

Shouldn't you be using PLINE?

.LINE Specify first point: Specify first point:

The previous code example assumes the CMDECHO system variable is set to 1 (On). If CMDECHO is set to 0 (Off), AutoCAD does not echo prompts during a `command`  function call. The following code uses the CMDECHO system variable to prevent the LINE command prompt from repeating:

```lisp
(defun C:LINE ( / cmdsave )
  (setq cmdsave (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (princ "Shouldn't you be using PLINE?\n")
  (command ".LINE")
  (setvar "cmdecho" cmdsave)
 (princ)
)

C:LINE
```

Now if you enter line at the AutoCAD Command prompt, the following text is displayed:

Shouldn't you be using PLINE?

Specify first point:

You can undefined and redefine commands to create a drawing management system, for example. You can redefine the NEW, OPEN, and QUIT commands to write billing information to a log file after a drawing is created and before you terminate an editing session.
