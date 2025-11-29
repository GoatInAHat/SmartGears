---
title: setfunhelp (AutoLISP)
guid: "GUID-61B2326A-8DE4-46C4-B9F9-22B8D163A761"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-61B2326A-8DE4-46C4-B9F9-22B8D163A761.htm"
generated: "2025-11-28T19:06:40.570989Z"
description: "Registers a user-defined command with the Help facility so the appropriate Help file and topic are called when the user requests help on that command"
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
  - function
---

# setfunhelp (AutoLISP)

> Registers a user-defined command with the Help facility so the appropriate Help file and topic are called when the user requests help on that command

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-61B2326A-8DE4-46C4-B9F9-22B8D163A761.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-61B2326A-8DE4-46C4-B9F9-22B8D163A761.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(setfunhelp
c:fname [helpfile [topic [command]]]
)
```

- ***c:fname*:** **Type:**  String  User-defined command (the `C:XXX` function). You must include the `c:`  prefix.
- ***helpfile*:** **Type:**  String  Help file name. The file extension is not required with the *helpfile*  argument. If a file extension is provided, AutoCAD looks only for a file with the exact name specified.  If no file extension is provided, AutoCAD looks for *helpfile*  with an extension of . *chm*. If no file of that name is found, AutoCAD looks for a file with an extension of . *hlp*.
- ***topic*:** **Type:**  String  Help topic ID. If you are calling a topic within a CHM file, provide the file name without the extension; AutoCAD adds an *.htm*  extension.
- ***command*:** **Type:**  String  Initial state of the Help window. The *command* argument is a string used by the uCommand (in HTML Help) or the fuCommand (in WinHelp) argument of the HtmlHelp() and WinHelp() functions as defined in the Microsoft Windows SDK.  For HTML Help files, the *command* parameter can be HH_ALINK_LOOKUP or HH_DISPLAY_TOPIC. For Windows Help files, the *command*  parameter can be HELP_CONTENTS, HELP_HELPONHELP, or HELP_PARTIALKEY.

## Return Values

**Type:**  String or nil

*c:fname*, if successful; otherwise, `nil`.

This function verifies only that the *c:fname*  argument has the `c:`  prefix. It does *not*  verify that the `c:fname`  function exists, nor does it verify the correctness of the other arguments supplied.

## Remarks

Note:
 This function is supported on Web, but does not affect the program since it doesn't support contextual help.

## Examples

The following example illustrates the use of `setfunhelp`  by defining a simple function and issuing `setfunhelp`  to associate the function with the Entget topic in the AutoCAD Help file (*acad.chm*):

```lisp
(defun c:foo ()
  (getstring "Press F1 for help on the foo command:")
)
(setfunhelp "c:test" "acad.chm" "entget")
```

After this code is loaded, issuing the `foo`  command and then pressing F1 displays the circle topic.

This example works, but serves no real purpose. In the real world, you would create your own Help file and associate that Help file and topic with your function.

Define a function named `test`:

```lisp
(defun c:test()(getstring "\nTEST: " )(princ))

C:TEST
```

Associate the function with a call to Help with the string “line”:

```lisp
(setfunhelp "c:test" "acad_acr.chm" "line")

"c:test"
```

Run the `test`  command and at the prompt, press F1; you should see the Help topic for the AutoCAD LINE command.

Note:
 When you use the
defun
 function to define a
C:XXX
 function, it removes that function's name from those registered by
setfunhelp
 (if one exists). Therefore,
setfunhelp
 should be called only after the
defun
 call, which defines the user-defined command.
