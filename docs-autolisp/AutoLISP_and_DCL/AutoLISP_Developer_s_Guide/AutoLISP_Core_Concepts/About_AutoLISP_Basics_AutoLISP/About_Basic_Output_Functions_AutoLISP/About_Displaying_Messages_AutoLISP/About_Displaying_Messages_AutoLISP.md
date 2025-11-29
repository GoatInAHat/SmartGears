---
title: About Displaying Messages (AutoLISP)
guid: "GUID-58AD9863-BB35-4982-9382-581FE8A5022C"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-58AD9863-BB35-4982-9382-581FE8A5022C.htm"
generated: "2025-11-28T19:06:03.231111Z"
description: AutoLISP programs often need to inform the user of an error or request for input.
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

# About Displaying Messages (AutoLISP)

> AutoLISP programs often need to inform the user of an error or request for input.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-58AD9863-BB35-4982-9382-581FE8A5022C.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-58AD9863-BB35-4982-9382-581FE8A5022C.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

Messages that are displayed should try and not interrupt the flow of a command, and when they do the text displayed should be short and precise as to what the problem is or the input that is being requested. AutoLISP offers the following functions to display messages to the user:

- prompt
   - Displays string at the AutoCAD Command prompt.
- princ
   - Displays a value at the AutoCAD Command prompt or to an open file. Strings are displayed without the enclosing quotation marks.
- prin1
   - Displays a value at the AutoCAD Command prompt or to an open file. Strings are enclosed in quotation marks.
- print
   - Displays a value at the AutoCAD Command prompt or to an open file, but a blank line is placed before the value and a space is placed after the value. Strings are enclosed in quotation marks.
- alert
   - Displays a dialog box containing an error or warning message.
- terpri
   - Prints a newline to the AutoCAD Command prompt.

The `write-char`, `write-line`, `get *XXX*`, and `entsel`  functions can also display messages at the AutoCAD Command prompt.

When entered from the Visual LISP Console window prompt, the prompt function displays a message (a string) in the AutoCAD Command window and returns nil to the Visual LISP Console window. The `princ`, `prin1`, and `print`  functions all display a value (not necessarily a string) in the AutoCAD Command prompt and returns the value to the Visual LISP Console window.

Note:
 The Visual LISP IDE is not available in AutoCAD LT for Windows and on Mac OS.

The following examples demonstrate the differences between the basic output functions and how they handle the same string of text.

```lisp
(setq str "The \"allowable\" tolerance is \261 \274\"")
(prompt str)

outputs
 The "allowable" tolerance is 1/4"

returns
 nil

(princ str)

outputs
 The "allowable" tolerance is 1/4"

returns
 "The \"allowable\" tolerance is 1/4\""

(prin1 str)

outputs
 "The \"allowable\" tolerance is 1/4""

returns
 "The \"allowable\" tolerance is 1/4\""

(print str)

outputs
<blank line> "The \"allowable\" tolerance is 1/4""<space>

returns
 "The \"allowable\" tolerance is 1/4\""
```
