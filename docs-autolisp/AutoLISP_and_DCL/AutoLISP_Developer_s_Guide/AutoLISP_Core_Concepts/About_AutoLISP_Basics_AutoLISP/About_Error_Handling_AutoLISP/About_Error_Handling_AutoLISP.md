---
title: About Error Handling (AutoLISP)
guid: "GUID-027AD2E0-5AC5-48DA-B451-112B7EECE40F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-027AD2E0-5AC5-48DA-B451-112B7EECE40F.htm"
generated: "2025-11-28T19:06:05.654204Z"
description: The AutoLISP language provides several functions for handling errors.
topic_type: concept
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

# About Error Handling (AutoLISP)

> The AutoLISP language provides several functions for handling errors.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-027AD2E0-5AC5-48DA-B451-112B7EECE40F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-027AD2E0-5AC5-48DA-B451-112B7EECE40F.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The proper handling of errors allows your program to exit gracefully and with an expected result. Using the error handling functions of the AutoLISP programming language allows you to do the following:

- Provide information to users when an error occurs during the execution of a program.
- Restore the AutoCAD environment to a known state.
- Intercept errors and continue program execution.

The following functions are useful to handle errors encountered by your programs:

- *error*
   - A user-definable error-handling function.
- *pop-error-mode*
   - Error-handling function that ends the previous call to
  *push-error-using-command*
   or
  *push-error-using-stack*
  .
- *push-error-using-command*
   - Error-handling function that indicates the use of the command function within a custom
  *error*
   handler.
- *push-error-using-stack*
   - Error-handling function that indicates the use of variables from the AutoLISP stack within a custom
  *error*
   handler.
- vl-catch-all-apply
   - Passes a list of arguments to a specified function and traps any exceptions.

If your program contains more than one error in the same expression, you cannot depend on the order in which AutoLISP detects the errors. For example, the `inters`  function requires several arguments, each of which must be either a 2D or 3D point list. A call to inters like the following:

```lisp
(inters 'a)
```

Two errors are encountered: too few arguments and invalid argument type. You will receive either of the following error messages:

```lisp
; *** ERROR: too few arguments
; *** ERROR: bad argument type: 2D/3D point
```

Your program should be designed to handle either error.

Note:
 AutoLISP evaluates all arguments before checking the argument types. In earlier releases of AutoCAD, AutoLISP evaluated and checked the type of each argument sequentially. To see the difference, look at the following code examples:

```lisp
(defun foo ()
  (print "Evaluating foo")
  '(1 2))

(defun bar ()
  (print "Evaluating bar")
  'b)

(defun baz ()
  (print "Evaluating baz")
  'c)
```

Observe how an expression using the `inters`  function is evaluated in AutoCAD:

Command: **(inters (foo) (bar) (baz))**

"Evaluating foo"

"Evaluating bar"

"Evaluating baz"

; *** ERROR: too few arguments

Each argument was evaluated successfully before AutoLISP passed the results to `inters`  and discovered that too few arguments were specified.

In AutoCAD R14 and earlier, the same expression evaluated as follows:

Command: **(inters (foo) (bar) (baz))**

"Evaluating foo"

"Evaluating bar" error: bad argument type

AutoLISP evaluated `(foo)`, then passed the result to `inters`. Since the result was a valid 2D point list, AutoLISP proceeds to evaluate `(bar)`, where it determines that the evaluated result is a string, an invalid argument type for `inters`.
