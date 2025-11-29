---
title: About Local and Global Variables (AutoLISP)
guid: "GUID-A8045F62-1CE3-4022-ACC1-CC0E2C1E158D"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A8045F62-1CE3-4022-ACC1-CC0E2C1E158D.htm"
generated: "2025-11-28T19:06:05.043077Z"
description: Variables can be local or global in scope based on how they are defined.
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

# About Local and Global Variables (AutoLISP)

> Variables can be local or global in scope based on how they are defined.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A8045F62-1CE3-4022-ACC1-CC0E2C1E158D.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A8045F62-1CE3-4022-ACC1-CC0E2C1E158D.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The use of local variables ensures that the variables in your functions are unaffected by other user-defined functions and custom applications. These variables do not remain available after the calling function has completed its task.

When you define a function or a command, any variables that you want to remain local must be added after the forward slash (**`/`**) in the arguments and local variables list. For example, the following example defines a function named `ARGTEST`  which combines a constant string with other strings values. `arg1`  and `arg2`  are populated by the arguments you provide when using the function, but `ccc`  is defined as a local variable for this function.

```lisp
(defun ARGTEST ( arg1 arg2 / ccc )
  (setq ccc "Constant string")
  (strcat ccc ", " arg1 ", " arg2)
)

ARGTEST

(ARGTEST "String 1" "String 2")

"Constant string, String 1, String 2"
```

Once the function is done, the value of `ccc`  is lost. You can test this by entering the following at the AutoCAD Command prompt:

```lisp
!ccc

nil
```

Tip:
 Do not make your variables local until after you have done most of the debugging for your function. By not declaring your variables as local right away, you can check the last values assigned to a variable after the function has finished.

Another advantage of using local variables is that AutoCAD can recycle the memory space used by these variables, whereas global variables keep accumulating within AutoCAD memory space.

Global variables can be helpful if you want to retain values in between the uses of a function or command while the function remains loaded, or to use a value across many functions. However, if all or many of your variables are global it becomes increasingly possible that you could end up changing the value of a variable so it is incompatible with another function. This can lead to unpredictable behavior and it can be very difficult to identify the source of a problem. When declaring a global variable, it is good practice to indicate that you intend a variable to be global. A common way of doing this is to add an opening and closing asterisk to the variable name, for example, `*default-layer*`.

All variables when they are initially declared are global. The following code demonstrates the use of both global and local variables.

```lisp
(
setq *dr-layer* "Doors")
(defun list-layers ( / cur-layer)
  (setq cur-layer (getvar "clayer"))
  (prompt (strcat "\nCurrent layer: " cur-layer "\nDoor layer: " *dr-layer*))
 (princ)
)

LIST-LAYERS

(list-layers)

Current layer: 0
Door layer: Doors
```

You can test the values stored in the variables by doing the following:

```lisp
!cur-layer

nil

!*dr-layer*

"Doors"
```

While a variable can be declared as local in a function, a variable with the same name can also be declared as global. If a variable name is added to the local variables list of a function, the global variable with the same name is ignored. The following example code demonstrates this behavior:

```lisp
(setq var-scope "Global")
(defun list-scope ( / var-scope)
  (if (/= var-scope nil)
    (prompt (strcat "\nScope: " var-scope))
    (prompt (strcat "\nvar-scope is nil"))
  )

  (setq var-scope "Local")
  (prompt (strcat "\nScope: " var-scope))
 (princ)
)

(list-scope)

var-scope is nil
Scope: Local

!var-scope

"Global"
```

When the function is started, the variable `var-scope`  is declared with a value of nil within the scope of the function. This is why the message `var-scope`  is `nil`  is returned when checking to see if the variable is `nil`. If `var-scope`  was not added to the local variables list for the function, the message Scope: Global would have been displayed and the value of `var-scope`  changed to "Local".

```lisp
(setq var-scope "Global")
(defun list-scope ( / )
  (if (/= var-scope nil)
    (prompt (strcat "\nScope: " var-scope))
    (prompt (strcat "\nvar-scope is nil"))
  )

  (setq var-scope "Local")
  (prompt (strcat "\nScope: " var-scope))
 (princ)
)

(list-scope)

Scope: Global
Scope: Local

!var-scope

"Local"
```
