---
title: About Control Characters in Strings (AutoLISP)
guid: "GUID-C8E55E82-0104-4305-95E5-1671628A2FF3"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-C8E55E82-0104-4305-95E5-1671628A2FF3.htm"
generated: "2025-11-28T19:06:03.421490Z"
description: Within quoted string values, the backslash (\) character allows control characters (or escape codes) to be included.
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

# About Control Characters in Strings (AutoLISP)

> Within quoted string values, the backslash (\) character allows control characters (or escape codes) to be included.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-C8E55E82-0104-4305-95E5-1671628A2FF3.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-C8E55E82-0104-4305-95E5-1671628A2FF3.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The following lists the currently recognized control characters:

| AutoLISP control characters |  |
| --- | --- |
| Code | Description |
| `\\` | \ character |
| `\"` | " character |
| `\e` | Escape character |
| `\n` | Newline character |
| `\r` | Return character |
| `\t` | Tab character |
| \ `*nnn*` | Character whose octal code is *nnn* |

The `prompt`, `princ`, and `get *XXX*`  functions expand the control characters in a string and display the expanded string at the AutoCAD Command prompt.

The following example shows displaying a backslash character (**\**) and quotation mark (**"**) within a quoted string:

```lisp
(princ "The \"filename\" is: D:\\ACAD\\TEST.TXT.")

The "filename" is: D:\ACAD\TEST.TXT
```

Text can be forced across multiple lines with the newline character (`\n`).

```lisp
(prompt "An example of the \nnewline character. ")

An example of the
newline character.
```

You can also use the `terpri`  function to cause a line break.

The Return character (`\r`) returns to the beginning of the current line. This is useful for displaying incremental information (for example, a counter showing the number of objects processed during a loop).

A Tab character (`\t`) can be used in strings to indent or to provide alignment with other tabbed text strings. In this example, note the use of the `princ`  function to suppress the ending `nil`.

```lisp
(prompt "\nName\tOffice\n- - - - -\t- - - - -

(_>
\nSue\t101\nJoe\t102\nSam\t103\n")(princ)

Name Office
- - - - - - - - - -
Sue 101
Joe 102
Sam 103
```
