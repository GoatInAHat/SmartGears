---
title: About Using AutoCAD Commands (AutoLISP)
guid: "GUID-75F19C76-78D0-443B-BC39-EB9FEB4650B1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-75F19C76-78D0-443B-BC39-EB9FEB4650B1.htm"
generated: "2025-11-28T19:06:06.340063Z"
description: "AutoLISP can execute a built-in AutoCAD command or one that is defined in a loaded ObjectARX or Managed .NET application."
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

# About Using AutoCAD Commands (AutoLISP)

> AutoLISP can execute a built-in AutoCAD command or one that is defined in a loaded ObjectARX or Managed .NET application.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-75F19C76-78D0-443B-BC39-EB9FEB4650B1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-75F19C76-78D0-443B-BC39-EB9FEB4650B1.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

Note:
 ObjectARX is not supported in AutoCAD LT, and Managed .NET is not supported in AutoCAD LT for Windows or on Mac OS.

The `command`  and `command-s`  functions allow you to start and pass values to an AutoCAD command. The `command`  and `command-s`  functions have a variable-length argument list. The first argument of these functions must be the command you want to execute. All other arguments must correspond to the types and values expected by that command's prompt sequence; these may be strings, real values, integers, points, entity names, or selection set names. Data such as angles, distances, and points can be passed either as strings or as the values themselves (as integer or real values, or as point lists). An empty string (`""`) is equivalent to pressing the Spacebar or Enter on the keyboard.

The `command-s`  function is faster and more efficient than the `command`  function, but the command being executed within the `command-s`  function must be completed within the same statement. This means that an argument must be provided for each of the command’s prompts, and that it cannot execute any more AutoLISP statements until the function has completed. Unlike the `command-s`  function, you can use AutoLISP functions within the `command`  function and the command that is being executed does not need to be completed to continue execution of the program.

There are some restrictions on the commands that you can use with the `command`  and `command-s`  functions.

The following code fragment shows representative calls to `command`.

```lisp
(defun c:CircC ()
  (command "._circle" "0,0" "3,3")
  (command "._thickness" 1)
  (command "._circle" PAUSE PAUSE)
 (princ)
)
```

When the `CircC`  command is loaded and executed at the AutoCAD Command prompt, the following actions occur:

1. The first call to
   command
    passes points to the CIRCLE command as strings (draws a circle centered at 0.0,0.0 and passes through 3.0,3.0).
2. The second call to command passes an integer to the THICKNESS system variable (changes the current thickness to 1.0).
3.  The last call to command prompts the user for a center point and then the radius of the circle.

The following code fragment shows representative calls to `command-s`.

```lisp
(defun c:CircCS ( / p1 rad)
  (command-s "._circle" "0,0" "3,3")
  (command-s "._thickness" 1)
  (setq p1 (getpoint "\nEnter a center point: "))
  (setq rad (getdist p1 "\nEnter a radius: "))
  (command-s "._circle" p1 rad)
 (princ)
)
```

The `CircCS`  command is similar to `CircC`  except it prompts the user for a center point and radius before making the last call to the `command-s`  function. With the `command-s`  function, you should avoid the use of the PAUSE token.
