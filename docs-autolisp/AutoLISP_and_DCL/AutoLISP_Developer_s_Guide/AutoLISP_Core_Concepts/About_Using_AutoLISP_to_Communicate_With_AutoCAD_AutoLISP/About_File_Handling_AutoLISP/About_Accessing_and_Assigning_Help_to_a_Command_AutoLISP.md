---
title: About Accessing and Assigning Help to a Command (AutoLISP)
guid: "GUID-02866F82-0BA8-4395-A4EA-9A90EC934045"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-02866F82-0BA8-4395-A4EA-9A90EC934045.htm"
generated: "2025-11-28T19:06:09.808167Z"
description: The help and setfunhelp functions provide access to the product and your custom help files.
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

# About Accessing and Assigning Help to a Command (AutoLISP)

> The help and setfunhelp functions provide access to the product and your custom help files.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-02866F82-0BA8-4395-A4EA-9A90EC934045.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-02866F82-0BA8-4395-A4EA-9A90EC934045.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The Help facility supports:

- HyperText Markup Language (.htm/.html) files - Windows and Mac OS
- Microsoft Compiled HTML Help (.chm) files - Windows only
- Windows Help (.hlp) files - Windows only

You can use the help function to display a Help file. Depending on the Help file's extension, the help function displays the appropriate viewer for the specified file. The following example code displays the LINE commands topic in the default AutoCAD Help file.

```lisp
(help "" "line")
```

You can create a custom Help file that provides information about your applications and display it with the help function. The following example code displays a custom help file named *abcindoorcad.chm*:

```lisp
(defun C:ABCINDOORCADHELP ()
  (help "abcindoorcad.chm")
 (princ)
)
```

The `setfunhelp`  function provides contextual help for user-defined commands. After the definition of a new command defined with AutoLISP, add a call to `setfunhelp`  and associate a specific help topic to the command. The following example assigns the help topic “MyCommand” in the *abcindoorcad.chm*  file to the user-defined MYCOMMAND command:

```lisp
(defun C:MYCOMMAND ( )
  ..

Command definition

  ..
)
(setfunhelp "c:mycommand" "abcindoorcad.chm" "mycommand")
```
