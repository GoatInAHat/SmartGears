---
title: "vl-cmdf (AutoLISP)"
guid: "GUID-6F1AAB6B-D5B1-426A-A463-0CBE93E4D956"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6F1AAB6B-D5B1-426A-A463-0CBE93E4D956.htm"
generated: "2025-11-28T19:06:45.790333Z"
description: Executes an AutoCAD command
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

# vl-cmdf (AutoLISP)

> Executes an AutoCAD command

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6F1AAB6B-D5B1-426A-A463-0CBE93E4D956.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-6F1AAB6B-D5B1-426A-A463-0CBE93E4D956.htm)
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
(vl-cmdf
[arguments ...]
)
```

- ***arguments*:** **Type:**  Integer, Real, String, List, or Ename (entity name)  AutoCAD commands and their options.  The *arguments*  to the `vl-cmdf`  function can be strings, reals, integers, or points, as expected by the prompt sequence of the executed command. A null string (`""`) is equivalent to pressing Enter on the keyboard. Invoking `vl-cmdf`  with no argument is equivalent to pressing Esc and cancels most AutoCAD commands.

## Return Values

**Type:**  T

Always returns `T`.

## Remarks

The `vl-cmdf`  function is similar to the `command`  function, but differs from `command`  in the way it evaluates the arguments passed to it. The `vl-cmdf`  function evaluates all the supplied arguments before executing the AutoCAD command, and will not execute the AutoCAD command if it detects an error during argument evaluation. In contrast, the `command`  function passes each argument in turn to AutoCAD, so the command may be partially executed before an error is detected.

If your command call includes a call to another function, `vl-cmdf`  executes the call *before*  it executes your command, while `command`  executes the call *after*  it begins executing your command.

Some AutoCAD commands may work correctly when invoked through `vl-cmdf`, while failing when invoked through `command`. The `vl-cmdf`  function mainly overcomes the limitation of not being able to use `get *XXX*`  functions inside command.

Note:
 If you issue
vl-cmdf
 from Visual LISP in AutoCAD on Windows, focus does not change to the AutoCAD window. If the command requires user input, you will see the return value (
T
) in the Console window, but AutoCAD will be waiting for input. You must manually activate the AutoCAD window and respond to the prompts. Until you do so, any subsequent commands will fail.

## Examples

The differences between `command`  and `vl-cmdf`  are easier to see if you enter the following calls at the AutoCAD Command prompt:

Command: **(command "._line" (getpoint "point?") '(0 0) "")**

line Specify first point: point?

Specify next point or [Undo]:

Command: nil

Using `command`, the LINE command executes first; then the `getpoint`  function is called.

Command: **(vl-cmdf "._line" (getpoint "point?") '(0 0) "")**

point?line Specify first point:

Specify next point or [Undo]:

Command: T

Using `vl-cmdf`, the `getpoint`  function is called first (notice the “point?” prompt from `getpoint`); then the LINE command executes.

The following examples show the same commands, but pass an invalid point list argument to the LINE command. Notice how the results differ:

Command: **(command "._line" (getpoint "point?") '(0) "")**

line Specify first point: point?

Specify next point or [Undo]:

Command: ERASE nil

Select objects: Specify opposite corner: *Cancel*

0 found

The `command`  function passes each argument in turn to AutoCAD, without evaluating the argument, so the invalid point list is undetected.

Command: **(vl-cmdf "._line" (getpoint "point?") '(0) "")**

point?Application ERROR: Invalid entity/point list.

nil

Because `vl-cmdf`  evaluates each argument before passing the command to AutoCAD, the invalid point list is detected and the command is not executed.
