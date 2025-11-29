---
title: About Defining a Function (AutoLISP)
guid: "GUID-9EDF48C2-1678-4DC3-BFD6-9D1DEAC525F0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9EDF48C2-1678-4DC3-BFD6-9D1DEAC525F0.htm"
generated: "2025-11-28T19:06:04.184432Z"
description: You can define your own functions.
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

# About Defining a Function (AutoLISP)

> You can define your own functions.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9EDF48C2-1678-4DC3-BFD6-9D1DEAC525F0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9EDF48C2-1678-4DC3-BFD6-9D1DEAC525F0.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

Once defined, these functions can be used at the AutoCAD Command prompt, the Visual LISP Console prompt, or within other AutoLISP expressions, just as you use the standard functions.

Note:
 The Visual LISP IDE is not available in AutoCAD LT for Windows and on Mac OS.

You can also create your own commands, because commands are just a special type of function. The `defun`  function combines a multiple expressions into a function or command. This function requires at least three arguments:

- Name of the function (symbol name)
- Argument list (a list of arguments and local variables used by the function). The argument list can be nil or an empty list ().
- AutoLISP expressions to execute with the function or command. There must be at least one expression in a function definition.

```lisp
(defun
symbol_name
 (
arguments
 /
local_variables
 )

expressions

)
```

The following example code defines a simple function that accepts no arguments and displays the message “bye” at the AutoCAD Command prompt. Note that the argument list is defined as an empty list (()):

```lisp
(defun DONE ( ) (prompt "\nbye! "))

DONE
```

Once the `DONE`  function is defined, you can use it as you would any other function. For example, the following code prints a message, then says “bye” at the AutoCAD Command prompt:

```lisp
(prompt "The value is 127.") (DONE) (princ)

The value is 127
bye!
```

Note how the previous example invokes the `princ`  function without an argument to suppress an ending nil and achieves a quiet exit.

Functions that accept no arguments may seem useless. However, you might use this type of function to query the state of certain system variables or conditions and to return a value that indicates those values.
