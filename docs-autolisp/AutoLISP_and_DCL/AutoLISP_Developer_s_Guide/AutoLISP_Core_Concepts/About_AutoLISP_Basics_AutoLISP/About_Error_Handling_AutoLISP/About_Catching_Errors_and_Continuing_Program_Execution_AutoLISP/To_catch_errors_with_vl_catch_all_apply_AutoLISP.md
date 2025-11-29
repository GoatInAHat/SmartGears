---
title: "To catch errors with vl-catch-all-apply (AutoLISP)"
guid: "GUID-2138B8F3-F0E1-4DB4-97C4-EC26047550E0"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2138B8F3-F0E1-4DB4-97C4-EC26047550E0.htm"
generated: "2025-11-28T19:06:05.988768Z"
description: Errors caused by AutoLISP functions can result in a program ending unexpectedly, make sure to handle all known situations that could cause an error.
topic_type: "task-adsk"
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

# To catch errors with vl-catch-all-apply (AutoLISP)

> Errors caused by AutoLISP functions can result in a program ending unexpectedly, make sure to handle all known situations that could cause an error.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2138B8F3-F0E1-4DB4-97C4-EC26047550E0.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-2138B8F3-F0E1-4DB4-97C4-EC26047550E0.htm)
- Topic Type: task-adsk
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

1. Wrap each function that could throw an error with
   vl-catch-all-apply
   .
2. Evaluate the value returned by
   vl-catch-all-apply
    with
   vl-catch-all-error-p
    to see if an error object or a value was returned.
3. Use
   vl-catch-all-error-message
    to get the message associated with the returned error object.
4. Load, run, and test the code.

## Example

The following defines a function named catch-me-if-you-can. This function accepts two number arguments and uses `vl-catch-all-apply`  to divide the first number by the second number. The `vl-catch-all-error-p`  function determines whether the return value from `vl-catch-all-apply`  is an error object. If the return value is an error object, catch-me-if-you-can invokes `vl-catch-all-error-message`  to obtain the message from the error object.

1. At the AutoCAD Command prompt, enter the following code:

   ```lisp
   (defun catch-me-if-you-can (dividend divisor / errobj)
     (setq errobj (vl-catch-all-apply '/ (list dividend divisor)))
     (if (vl-catch-all-error-p errobj)
       (progn
         (print (strcat "An error occurred: " (vl-catch-all-error-message errobj)))
         (initget "Yes No")
         (setq ans (getkword "Do you want to continue? [Y/N]: "))
         (if (equal (strcase ans) "YES")
           (print "Okay, I'll keep going")
         )
       )
       (print errobj)
     )
    (princ)
   )
   ```

   Note:
    You can also add the example code to an existing or create a new LSP file. Then load the LSP file with the APPLOAD command.
2. Enter the following code:

   ```lisp
   (catch-me-if-you-can 50 2)
   ```

   The function returns 25.
3. Enter the following code:

   ```lisp
   (catch-me-if-you-can 50 0)
   ```

   The function issues the following prompt:

   "An error occurred: divide by zero" Do you want to continue? [Y/N]:

   If you enter y (or yes), catch-me-if-you-can indicates that it will continue processing. Try modifying this example by changing `vl-catch-all-apply`  to `apply`. Load and run the example with a divide by zero again. When apply results in an error, execution immediately halts and *error* is called, resulting in an error message.

   The `vl-catch-*`  functions are especially important when you use ActiveX with AutoLISP. Many of the AutoCAD ActiveX automation methods are designed to be used in the “programming by exception” style. This means they either return useful values if they succeed, or raise an exception if they fail (instead of returning an error value). If your program uses ActiveX methods, you must prepare it to catch exceptions, otherwise the program halts, leaving the user at a Command prompt.

   Note:
    ActiveX is not supported on Mac OS and Web.
