---
title: "*push-error-using-stack* (AutoLISP)"
guid: "GUID-C28420C9-2210-4EEC-AA73-2962999D1BC1"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C28420C9-2210-4EEC-AA73-2962999D1BC1.htm"
generated: "2025-11-28T19:06:20.447903Z"
description: "Error-handling function that indicates the use of variables from the AutoLISP stack within a custom *error* handler"
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

# *push-error-using-stack* (AutoLISP)

> Error-handling function that indicates the use of variables from the AutoLISP stack within a custom *error* handler

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C28420C9-2210-4EEC-AA73-2962999D1BC1.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-C28420C9-2210-4EEC-AA73-2962999D1BC1.htm)
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
(*push-error-using-stack*)
```

No arguments.

## Return Values

**Type:**  T

A value of `T`  is returned.

## Remarks

Allows access to the local AutoLISP variables on the stack defined within the function where the error occurred from your custom `*error*`  handler. A call to the `*push-error-using-stack*`  function overrides a previous call to `*push-error-using-command*`.

If `*push-error-using-command*`  or `*push-error-using-stack*`  are not called, by default AutoLISP works as if `*push-error-using-stack*`  was called.

Note:
 This function cannot be used when the
command
 function is used within the local custom
*error*
 handler.

## Examples

The following example demonstrates the use of the `*push-error-using-stack*`  function.

```lisp
(setq var1 "Global1"      ; Set some global variables
      var2 "Global2")

(defun mySub_err (err_msg)
    (if (/= err_msg "Function cancelled")
      (progn
        (prompt (strcat "\nError: " err_msg))
        (prompt (strcat "\nLocalSub1: " var1))
        (prompt (strcat "\nLocalSub2: " var2))
        (terpri)
      )
    )

    (command-s "._undo" "_e")
    (command-s "._U")
    (setq *error* olderr)
  (princ)
)

(defun subUtil (val / olderr var1 var2)
    (*push-error-using-stack*)         ; Indicates if the custom error handler has access to local
                                       ; variables defined in his function

    (setq olderr *error*
          *error* mySub_err)

    (command "._undo" "_group")        ; The following will not be executed in this sample, but is good
                                       ; framework for setting up your own error handlers

    (setq var1 "Sub1"                  ; Set some local variables
          var2 "Sub2")

    ;; Perform your tasks here
    (strcat "Foo" val)

    (command "._undo" "_e")
    (setq *error* olderr)              ; Restore old *error* handler
    (*pop-error-mode*)                 ; End the use of *push-error-using-command*
)

(defun my_err (err_msg)
    (if (/= err_msg "Function cancelled")
      (progn
        (prompt (strcat "\nError: " err_msg))
        (prompt (strcat "\nLocal1: " var1))
        (prompt (strcat "\nLocal2: " var2))
        (terpri)
      )
    )

    (command "._undo" "_e")
    (command "._U")
    (setq *error* olderr)
  (princ)
)

(defun myUtil (val / var1 var2)
    (setq olderr *error*
          *error* my_err)

    (*push-error-using-command*)       ; Indicate use of Command function instead of Command-s
                                       ; in the custom error handler

    (setq var1 "Local1"                ; Set some local variables
          var2 "Local2")

    (command "._undo" "_group")        ; The following will not be executed in this sample, but is good
                                       ; framework for setting up your own error handlers

    (subUtil val)                      ; Call to a custom function that uses *push-error-using-stack*

    (/ 1 0)                            ; Call a function with incorrect values to trigger the custom error handler
                                       ; Remove when setting up your code

    ;; Perform your tasks here

    (command "._undo" "_e")
    (setq *error* olderr)              ; Restore old *error* handler
    (*pop-error-mode*)                 ; End the use of *push-error-using-command*
)
```

After loading the sample code, enter **(myutil 1)**  at the Command prompt to enter the error handler for the nested function and **(myutil “String”)**  to test the error handler of the main function.
