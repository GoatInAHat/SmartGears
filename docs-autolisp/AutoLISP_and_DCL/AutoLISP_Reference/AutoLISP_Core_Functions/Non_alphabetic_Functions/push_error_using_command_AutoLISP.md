---
title: "*push-error-using-command* (AutoLISP)"
guid: "GUID-620E034A-9151-427F-B6F5-B360D14DA925"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-620E034A-9151-427F-B6F5-B360D14DA925.htm"
generated: "2025-11-28T19:06:20.345442Z"
description: "Error-handling function that indicates the use of the command function within a custom *error* handler"
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

# *push-error-using-command* (AutoLISP)

> Error-handling function that indicates the use of the command function within a custom *error* handler

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-620E034A-9151-427F-B6F5-B360D14DA925.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-620E034A-9151-427F-B6F5-B360D14DA925.htm)
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
(*push-error-using-command*)
```

No arguments.

## Return Values

**Type:**  T

A value of `T`  is returned.

## Remarks

A call to `*push-error-using-command*`  should be made after you define a custom `*error*`  handler function that contains the use of the `command`  function.

When using `*push-error-using-command*`, you are limited to accessing only global variables and none of the local variables defined within the function where the error occurred from your custom `*error*`  handler. If access to the local variables is required, replace any instances of the `command`  function with `command-s`  function and call `*push-error-using-stack*`  instead.

Note:
 This function cannot be used when the
command
 function is used within the local custom
*error*
 handler.

## Examples

The following example demonstrates the use of the `*push-error-using-command*`  function.

```lisp
(defun my_err (err_msg)
    (if (/= err_msg "Function cancelled")
      (prompt (strcat "\nError: " err_msg))
    )
    (command "._undo" "_e")
    (command "._U")
    (setq *error* olderr)
  (princ)
)

(defun myUtil (key / )
    (setq olderr *error*
              *error* my_err)
    (*push-error-using-command*)         ; Indicate use of Command function instead of Command-s
                                         ; in the custom error handler

    (command "._undo" "_group")          ; The following will not be executed in this sample, but is good
                                         ; framework for setting up your own error handlers

    (/ 1 0)                              ; Call a function with incorrect values to trigger the custom error handler
                                         ; Remove when setting up your code

    ;; Perform your tasks here

    (command "._undo" "_e")
    (setq *error* olderr)                ; Restore old *error* handler
    (*pop-error-mode*)                   ; End the use of *push-error-using-command*
)
```

After loading the sample code, enter **(myutil “String”)**  at the Command prompt to enter the error handler.
