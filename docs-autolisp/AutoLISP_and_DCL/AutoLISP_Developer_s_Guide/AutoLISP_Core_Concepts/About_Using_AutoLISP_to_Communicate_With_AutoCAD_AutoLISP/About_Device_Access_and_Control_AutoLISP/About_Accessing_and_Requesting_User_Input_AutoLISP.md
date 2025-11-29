---
title: About Accessing and Requesting User Input (AutoLISP)
guid: "GUID-7F024C1B-EFB8-4F6C-9EFE-A6491210A4CD"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7F024C1B-EFB8-4F6C-9EFE-A6491210A4CD.htm"
generated: "2025-11-28T19:06:10.038103Z"
description: AutoLISP can collect raw input from an input device, in addition to offering a set of functions designed to request specific types of input from the user.
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

# About Accessing and Requesting User Input (AutoLISP)

> AutoLISP can collect raw input from an input device, in addition to offering a set of functions designed to request specific types of input from the user.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7F024C1B-EFB8-4F6C-9EFE-A6491210A4CD.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-7F024C1B-EFB8-4F6C-9EFE-A6491210A4CD.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The following are some of the functions that can be used to get input from the user:

- grread
   – Reads values from any of the AutoCAD input devices.
- initget
   – Establishes keywords and controls certain types of input for use by the next user-input function call.
- getstring
   – Pauses for the input of a string and returns that string.
- getpoint
   – Pauses for the input of a point and returns that point.
- getint
   – Pauses for the input of an integer and returns that integer.
- getdist
   – Pauses for the input of a distance and returns a real number.
- getangle
   – Pauses for the input of an angle and returns the angle expressed in radians.

## Getting Direct Keyboard and Mouse Input

The `grread`  function returns raw user input, whether from the keyboard or from the pointing device (mouse or digitizer). If the call to `grread`  enables tracking, the function returns a digitized coordinate that can be used for things such as dragging. The value returned by `grread`  is a list and the first character defines the type of input that the user provided.

Note:
 There is no guarantee that applications calling
grread
 will be upward compatible. Because it depends on the current hardware configuration, applications that call
grread
 are not likely to work in the same way on all configurations.

The following example code uses `grread`  and checks to see if the input provided was from the keyboard.

```lisp
(defun c:GetCharacter ( / code)
  (prompt "\nEnter a single character: ")

  (setq code (grread))

  (if (= 2 (car code))
    (progn
      (prompt (strcat "\nCharacter entered was: " (chr (cadr code))))
      (prompt (strcat "\nASCII code: " (itoa (cadr code))))
    )
    (prompt "\nInput was not from the keyboard.")
  )
 (princ)
)
```

Loading and running the example code results in the following prompt being displayed:

Enter a single character:

Pressing a key on the keyboard displays the character and ASCII code of the key at the AutoCAD Command prompt. For example, the following is displayed if you press the F key when prompted for a single character and Caps Lock is not enabled or Shift is not held down:

Character entered was: f

ASCII code: 102

## Requesting Input with the GetXXX Functions

AutoLISP provides several functions to get basic input from the user at the AutoCAD Command prompt. These functions allow you to request get points, enter text or numbers, and even use keywords to make branching commands. Each user-input `getXXX`  function pauses for data entry of the indicated type and returns the value entered. The application calling one of the functions can specify an optional prompt to display before the function pauses for input. The `initget`  function does not work with all `getXXX`  functions.

The `initget`  function can be used to control the next call to a `getXXX`  function. This function accepts two arguments, bits and keywords, both of which are optional. The bits argument specifies one or more control bits that enable or disable certain input values to the next user-input function call. The keywords argument specifies one or more keywords that the next `getXXX`  function call will recognize. The control bits and keywords established by `initget`  apply only to the next `getXXX`  function call and do not need to be discarded after that call.

The following code uses the `getint`  function to prompt the user for an integer:

```lisp
(defun c:AskForInteger ( / )
  (setq int (getint "\nEnter an integer: "))

  (if int
    (prompt (strcat "\nUser entered: " (itoa int)))
    (prompt "\nUser did not provide an integer.")
  )
 (princ)
)
```

Loading and running the example code results in the following prompt being displayed:

Enter an integer:

Providing a valid integer returns the value entered for the `getint`  function and that value is displayed as part of the prompt “User entered:” at the AutoCAD Command prompt, but if an invalid integer is provided the message “Requires an integer value.” is displayed and the user is requested to provide an integer again. If Enter is pressed before a value is typed, the message “User did not provide an integer.” is displayed.

## Validating Input

You should protect your code from unintentional user errors. The AutoLISP user input `getXXX`  functions do much of this for you. However, it is important to check for adherence to other program requirements that the `getXXX`  functions do not check for. If you neglect to check input validity, the program's integrity can be seriously affected.
