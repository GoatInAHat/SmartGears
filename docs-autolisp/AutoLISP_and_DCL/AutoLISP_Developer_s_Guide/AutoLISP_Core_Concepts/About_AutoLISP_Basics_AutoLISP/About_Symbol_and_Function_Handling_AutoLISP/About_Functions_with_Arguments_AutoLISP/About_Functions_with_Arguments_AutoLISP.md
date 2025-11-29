---
title: About Functions with Arguments (AutoLISP)
guid: "GUID-31B647C5-61F3-4B06-BC88-4CE83EB981C5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-31B647C5-61F3-4B06-BC88-4CE83EB981C5.htm"
generated: "2025-11-28T19:06:05.417598Z"
description: With AutoLISP, many functions require you to pass them values. These values are known as arguments.
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

# About Functions with Arguments (AutoLISP)

> With AutoLISP, many functions require you to pass them values. These values are known as arguments.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-31B647C5-61F3-4B06-BC88-4CE83EB981C5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-31B647C5-61F3-4B06-BC88-4CE83EB981C5.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

There are functions that also accept no arguments, and some in which accept optional arguments. User-defined functions cannot have optional arguments. When you call a user-defined function that accepts arguments, you must provide values for all arguments.

Note:
 You can define multiple user functions with the same name, but have each definition accept a different number or type of arguments.

The symbols used as arguments are defined in the argument list before the local variables. Arguments are treated as a special type of local variable; argument variables are not available outside the function. You cannot define a function with multiple arguments of the same name.

If you do use the same name for multiple arguments, the following error message is displayed at the AutoCAD Command prompt:

duplicate argument name:

The following code defines a function that accepts two arguments. The code expects the arguments to both be of the string data type. The arguments are combined and returned as the resulting string.

```lisp
(defun ARGTEST ( arg1 arg2 / ccc )
  (setq ccc "Constant string")
  (strcat ccc ", " arg1 ", " arg2)
)

ARGTEST
```

The `ARGTEST`  function returns the desired value because AutoLISP always returns the results of the last expression it evaluates. The last line in `ARGTEST`  uses `strcat`  to concatenate the strings, and the resulting value is returned. This is one example where you should not use the `princ`  function to suppress the return value from your program.

This type of function can be used a number of times within an application to combine two variable strings with one constant string in a specific order. Because it returns a value, you can save the value to a variable for use later in the application.

```lisp
(setq newstr (ARGTEST "String 1" "String 2"))

"Constant string, String 1, String 2"
```

The `newstr`  variable is now set to the value of the three strings combined.

Note that the `ccc` variable was defined locally within the `ARGTEST`  function. Once the function runs to completion, AutoLISP recaptures the memory allocated to the variable. You can use the following code to check the value assigned to `ccc`.

```lisp
!ccc

nil
```

If string values are not passed to the `ARGTEST`  function, the `strcat`  function will return the following error:

; error: bad argument type: stringp 1

You can use the type function to verify the data type of an argument and respond appropriately. The `vl-catch-apply-all`  function could also be helpful in catching the error returned by the `strcat`  function. The following example code uses the `type`  function to make sure that the `ARGTEST`  function was passed two string values before trying to combine and return the resulting string.

```lisp
(defun ARGTEST (arg1 arg2 / ccc retVal)
  (setq ccc "Constant string")

  (if (= (type arg1) 'STR)
    (if (= (type arg2) 'STR)
      (setq retVal (strcat ccc ", " arg1 ", " arg2))
      (prompt "bad argument: arg2 not a string\n")
    )
    (prompt "bad argument: arg1 not a string\n")
  )
  (if retVal
    retVal
    (princ)
  )
)
```
