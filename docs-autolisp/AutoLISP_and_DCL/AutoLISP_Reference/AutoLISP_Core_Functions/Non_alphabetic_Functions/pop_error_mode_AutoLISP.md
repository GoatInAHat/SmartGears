---
title: "*pop-error-mode* (AutoLISP)"
guid: "GUID-60AFEFAD-1DF5-448F-A2E9-D7260DB279E4"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-60AFEFAD-1DF5-448F-A2E9-D7260DB279E4.htm"
generated: "2025-11-28T19:06:20.256681Z"
description: "Error-handling function that ends the previous call to *push-error-using-command* or *push-error-using-stack*"
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

# *pop-error-mode* (AutoLISP)

> Error-handling function that ends the previous call to *push-error-using-command* or *push-error-using-stack*

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-60AFEFAD-1DF5-448F-A2E9-D7260DB279E4.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-60AFEFAD-1DF5-448F-A2E9-D7260DB279E4.htm)
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
(*pop-error-mode*)
```

No arguments.

## Return Values

**Type:**  T

A value of `T`  is returned.

## Remarks

A call to `*pop-error-mode*`  should be made after replacing a custom `*error*`  handler function with the previously defined `*error*`  handler.

Note:
 This function is not required when using the
command-s
 function in an
*error*
 handler.

## Examples

The following example demonstrates the use of the `*pop-error-mode*`  function.

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
    (*push-error-using-command*)         ; Indicate use of the command function instead of command-s
                                         ; in a custom error handler

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
