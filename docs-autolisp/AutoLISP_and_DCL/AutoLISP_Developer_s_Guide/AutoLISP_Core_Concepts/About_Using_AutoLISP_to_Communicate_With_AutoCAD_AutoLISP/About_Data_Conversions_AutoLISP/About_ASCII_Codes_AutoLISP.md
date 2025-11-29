---
title: About ASCII Codes (AutoLISP)
guid: "GUID-22EE1BF1-C71B-4FDB-9C89-3697EDF93688"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-22EE1BF1-C71B-4FDB-9C89-3697EDF93688.htm"
generated: "2025-11-28T19:06:09.120236Z"
description: ASCII codes are integer values that represent alphanumeric characters.
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

# About ASCII Codes (AutoLISP)

> ASCII codes are integer values that represent alphanumeric characters.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-22EE1BF1-C71B-4FDB-9C89-3697EDF93688.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-22EE1BF1-C71B-4FDB-9C89-3697EDF93688.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Numeric values 0 through 127 represent digits 0 through 9, letters of the alphabet, punctuation marks, and other special characters. Some AutoLISP functions return or expect an ASCII code for a single character instead of the string equivalent.

For example, the `grread`  function returns the ASCII code of the key pressed when a user provides keyboard input. The ASCII code in the list returned can then be evaluated or converted for display based on how the program is going to use it.

The following are a few of the AutoLISP functions that are used when working with ASCII codes:

- grread
   – Reads values from any of the AutoCAD input devices.
- read-char
   – Returns the ASCII code representing the character read from the keyboard input buffer or an open file.
- write-char
   – Writes one character to the screen or to an open file.
- ascii
   – Returns the ASCII code for the first character of a string.
- chr
   – Converts an integer representing an ASCII character code into a single-character string.

## Converting ASCII Codes and Characters to ASCII Codes

The `ascii`  and `chr`  functions can be used to handle the conversion of ASCII codes. The `ascii`  function returns the ASCII code associated with a character string, and `chr`  returns the character string associated with an ASCII code.

The following code converts an uppercase A to its ASCII code value:

```lisp
(ascii "A")

65
```

The following code converts an ASCII code value to the character string it represents:

```lisp
(chr 65)

"A"
```

The following example code defines a command named `ASCII`  and a function named `BASE`  that is used by the `ASCII`  command. The `ASCII`  command writes characters with their codes in decimal, octal, and hexadecimal form to the screen and a file named *ASCII.txt*.

```lisp
; BASE converts from a decimal integer to a string in another base.
(defun BASE (bas int / ret yyy zot)
  (defun zot (i1 i2 / xxx)
    (if (> (setq xxx (rem i2 i1)) 9)
      (chr (+ 55 xxx))
      (itoa xxx)
    )
  )

  (setq ret (zot bas int)
        yyy (/ int bas)
  )
  (setq ret (strcat (zot bas yyy) ret))
  (setq yyy (/ yyy bas))

  (strcat (zot bas yyy) ret)
)

(defun C:ASCII (/)                      ;chk out ct code dec oct hex )
  (initget "Yes")
  (setq chk (getkword "\nWriting to ASCII.TXT, continue? <Y>: "))
  (if (or (= chk "Yes") (= chk nil))
    (progn
      (setq out  (open "ascii.txt" "w")
            chk  1
            code 0
            ct   0
      )
      (princ "\n \n CHAR DEC OCT HEX \n")
      (princ "\n \n CHAR DEC OCT HEX \n" out)
      (while chk
        (setq dec (strcat "  " (itoa code))
              oct (base 8 code)
              hex (base 16 code)
        )
        (setq dec (substr dec (- (strlen dec) 2) 3))
        (if (< (strlen oct) 3)
          (setq oct (strcat "0" oct))
        )

        (princ (strcat "\n " (chr code) " " dec " " oct " " hex))
        (princ (strcat "\n " (chr code) " " dec " " oct " " hex)
               out
        )

        (cond
          ((= code 255) (setq chk nil))
          ((= ct 20)
           (setq
             xxx (getstring
                   "\n \nPress 'X' to eXit or any key to continue: "
                 )
           )
           (if (= (strcase xxx) "X")
             (setq chk nil)
             (progn
               (setq ct 0)
               (princ "\n \n CHAR DEC OCT HEX \n")
             )
           )
          )
        )
        (setq ct   (1+ ct)
              code (1+ code)
        )
      )
      (close out)
      (setq out nil)
    )
  )
 (princ)
)
```
