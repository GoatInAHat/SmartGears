---
title: "About C:XXX Functions (AutoLISP)"
guid: "GUID-CBF72C4D-0197-4B42-B890-F72BE9E3CBD4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-CBF72C4D-0197-4B42-B890-F72BE9E3CBD4.htm"
generated: "2025-11-28T19:06:04.532702Z"
description: "If an AutoLISP function is defined with a name of the form C:xxx, it can be issued at the AutoCAD Command prompt in the same manner as a built-in AutoCAD command."
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# About C:XXX Functions (AutoLISP)

> If an AutoLISP function is defined with a name of the form C: xxx , it can be issued at the AutoCAD Command prompt in the same manner as a built-in AutoCAD command.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-CBF72C4D-0197-4B42-B890-F72BE9E3CBD4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-CBF72C4D-0197-4B42-B890-F72BE9E3CBD4.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

This is true regardless of whether you define and load the function in Visual LISP or at the AutoCAD Command prompt. You can use this feature to add new commands to AutoCAD or to redefine existing commands.

Note:
 The Visual LISP IDE is not available in AutoCAD LT for Windows and on Mac OS.

To use functions as AutoCAD commands, be sure they adhere to the following rules:

- The function name must use the form
  C:
  xxx
   (upper- or lowercase characters). The
  C:
   portion of the name must always be present; the

  XXX

   portion is a command name of your choice.
  C:
  xxx
   functions can be used to override built-in AutoCAD commands. (See About Redefining AutoCAD Commands [AutoLISP].)
- The function must be defined with no arguments. However, local variables are permitted and it is a good programming practice to use them.

A function defined in this manner can be issued transparently from within any prompt of any built-in AutoCAD command, provided the function issued transparently does not call the `command`  function. When issuing a `C: *xxx*`  defined command transparently, you must precede the `*XXX*`  portion with a single quotation mark (**'**).

You can issue a built-in command transparently while a `C: *xxx*`  command is active by preceding it with a single quotation mark (**'**), as you would with all commands that are issued transparently. However, you cannot issue a `C: *xxx*`  command transparently while a `C: *xxx*`  command is active.

Note:
 When calling a function defined as a command from the code of another AutoLISP function, you must use the whole name, including the parentheses; for example,
(C:HELLO)
. You also must use the whole name and the parentheses when you invoke the function from the VLISP Console prompt.
