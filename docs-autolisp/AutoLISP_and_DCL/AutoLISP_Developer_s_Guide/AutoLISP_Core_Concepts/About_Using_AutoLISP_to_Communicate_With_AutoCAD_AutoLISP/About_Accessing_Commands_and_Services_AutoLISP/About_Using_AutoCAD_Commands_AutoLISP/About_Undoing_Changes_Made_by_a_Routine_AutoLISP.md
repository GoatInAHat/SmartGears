---
title: About Undoing Changes Made by a Routine (AutoLISP)
guid: "GUID-4481039B-77DA-4500-AE8B-3D2AD6951115"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4481039B-77DA-4500-AE8B-3D2AD6951115.htm"
generated: "2025-11-28T19:06:06.741411Z"
description: Grouping multiple AutoLISP statements together under a single UNDO group allows you to rollback all the actions performed by using either the AutoCAD U or UNDO commands.
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

# About Undoing Changes Made by a Routine (AutoLISP)

> Grouping multiple AutoLISP statements together under a single UNDO group allows you to rollback all the actions performed by using either the AutoCAD U or UNDO commands.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4481039B-77DA-4500-AE8B-3D2AD6951115.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-4481039B-77DA-4500-AE8B-3D2AD6951115.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Each command executed with the `command`  and `command-s`  functions explicitly creates its own UNDO group. If a user enters U (or UNDO) at the AutoCAD Command prompt after running an AutoLISP routine, only the last command will be undone. Additional uses of UNDO will step backward further through the commands used in that routine. Users of your routine will expect that all of the operations that it performs can be undone in a single operation, instead of having to undo multiple operations to get back to the previous state of the drawing.

It is recommended to group the commands and operations performed by an AutoLISP routine into a single UNDO group, or if your routine allows the user to repeat operations you might even create UNDO groups for each time the user’s response affects the drawing. You can define an UNDO group by using the Begin and End options of the AutoCAD UNDO command.

The following example code demonstrates how each command executed with the command function has its own UNDO group.

```lisp
(defun c:NoUndo ( / old_osmode el pt)
  (setq old_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "._circle" "5,5" "2")    ;Draws a circle
  (command "._line" "3,5" "7,5" "") ;Draws a line
  (setq el (entlast))               ;Gets the last entity added
                                    ; to the drawing
  (setq pt '(5 7))                  ;Sets the trim point
  (command "._trim" el "" pt "")    ;Performs the trim
  (setvar "OSMODE" old_osmode)
)
```

After running the `c:NoUndo`  routine, you will see a semi-circle. Issuing the U or UNDO command after running the `c:NoUndo`  routine results in the AutoCAD TRIM command being undone; you should now see a full circle with a line running through its center. Executing the U or UNDO command again results in the line being undone that was created with the AutoCAD LINE command. A third use of the U or UNDO command results in the AutoCAD CIRCLE command being undone.

The following example code demonstrates how the AutoCAD UNDO command can be used to create an UNDO group that allows the user to rollback all changes back with a single U (or UNDO) command.

```lisp
(defun c:YesUndo ( / old_osmode el pt)
  (command "._UNDO" "_Begin")
  (setq old_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "._circle" "5,5" "2")    ;Draws a circle
  (command "._line" "3,5" "7,5" "") ;Draws a line
  (setq el (entlast))               ;Gets the last entity added
                                    ; to the drawing
  (setq pt '(5 7))                  ;Sets the trim point
  (command "._trim" el "" pt "")    ;Performs the trim
  (setvar "OSMODE" old_osmode)
  (command "._UNDO" "_End")
)
```

After running the `c:YesUndo`  routine, you will see a semi-circle just like with the `c:NoUndo`  routine. Issuing the U or UNDO command after running the `c:YesUndo`  routine results in the AutoCAD TRIM, LINE, and CIRCLE commands being undone.
