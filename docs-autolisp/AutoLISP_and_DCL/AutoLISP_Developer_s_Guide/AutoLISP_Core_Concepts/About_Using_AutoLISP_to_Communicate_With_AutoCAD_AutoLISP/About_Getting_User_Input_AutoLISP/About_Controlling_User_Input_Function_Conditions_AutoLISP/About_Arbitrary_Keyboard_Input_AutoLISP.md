---
title: About Arbitrary Keyboard Input (AutoLISP)
guid: "GUID-3967F2A7-D347-4A42-9162-6E1E45399AA9"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3967F2A7-D347-4A42-9162-6E1E45399AA9.htm"
generated: "2025-11-28T19:06:08.028147Z"
description: Arbitrary input allows you to provide a string to most of the getXXX functions as if it is a keyword; control bits and keywords are honored first.
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

# About Arbitrary Keyboard Input (AutoLISP)

> Arbitrary input allows you to provide a string to most of the getXXX functions as if it is a keyword; control bits and keywords are honored first.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3967F2A7-D347-4A42-9162-6E1E45399AA9.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-3967F2A7-D347-4A42-9162-6E1E45399AA9.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

An application using this facility can be written to permit the user to call an AutoLISP function at a `getXXX`  function prompt. You enable arbitrary keyboard input by using the 128 control bit with the `initget`  function.

The following example code defines a command named `ARBENTRY`  and a function named `REF`. The `REF`  function is used in response to the `getpoint`  function in the `ARBENTRY`  command which is set to allow arbitrary keyboard input.

```lisp
(defun C:ARBENTRY ( / pt1)
  (initget 128)                     ; Sets arbitrary entry bit
  (setq pt1 (getpoint "\nPoint: ")) ; Gets value from user.
  (if (= 'STR (type pt1))           ; If it's a string, convert it
    (setq pt1 (eval (read pt1)))    ; to a symbol, try evaluating
                                    ; it as a function; otherwise,
    pt1                             ; just return the value.
  )
)

(defun REF ( )
  (setvar "LASTPOINT" (getpoint "\nReference point: "))
  (getpoint "\nNext point: " (getvar "LASTPOINT"))
)
```

The following command sequence demonstrates how you can use `ARBENTRY`  and `REF`  together:

Command: **arbentry**

Point: **(ref)**

Reference point: *Select a point*

Next point: **@1,1,0**
