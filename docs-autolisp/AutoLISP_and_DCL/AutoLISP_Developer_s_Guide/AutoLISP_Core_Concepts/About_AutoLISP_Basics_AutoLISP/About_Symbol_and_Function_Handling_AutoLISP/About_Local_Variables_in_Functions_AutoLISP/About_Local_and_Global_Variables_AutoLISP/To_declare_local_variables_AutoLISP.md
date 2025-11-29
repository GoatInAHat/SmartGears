---
title: To declare local variables (AutoLISP)
guid: "GUID-89387281-599B-493F-AA3D-85DA4D6029B6"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-89387281-599B-493F-AA3D-85DA4D6029B6.htm"
generated: "2025-11-28T19:06:05.246766Z"
description: "Local variables are only accessible within the user-defined function that they are defined in."
topic_type: "task-adsk"
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

# To declare local variables (AutoLISP)

> Local variables are only accessible within the user-defined function that they are defined in.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-89387281-599B-493F-AA3D-85DA4D6029B6.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-89387281-599B-493F-AA3D-85DA4D6029B6.htm)
- Topic Type: task-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

1. In the arguments and variables list of the
   defun
    function, add a forward slash (

   /

   ) delimiter to the list.
2. After the forward slash, list each local variable.

   Be certain there is at least one space between the slash and each local variable.

## Example

The `LOCAL`  function in the following example defines two local variables: `aaa`  and `bbb`.

1. At the AutoCAD Command prompt, enter the following code:

   ```lisp
   (defun LOCAL ( / aaa bbb)
     (setq aaa "A" bbb "B")
     (princ (strcat "\naaa has the value " aaa ))
     (princ (strcat "\nbbb has the value " bbb))
     (princ)
   )
   LOCAL
   ```

   Note:
    You can also add the example code to an existing or create a new LSP file. Then load the LSP file with the APPLOAD command.
2. Enter the following code to define two global variables before using the
   LOCAL
    function:

   ```lisp
   (setq aaa 1 bbb 2)

   2
   ```
3. Enter the following code to check the value of the two global variables:

   ```lisp
   !aaa

   1

   !bbb

   2
   ```
4. Enter the following code to check the value of the two local variables:

   ```lisp
   (local)

   aaa has the value A
   bbb has the value B
   ```

   You will notice the function used the values for `aaa`  and `bbb`  that are local within the function. The current values for `aaa`  and `bbb`  are still set to their global values and can be verified with the following statements:

   ```lisp
   !aaa

   1

   !bbb

   2
   ```
