---
title: prin1 (AutoLISP)
guid: "GUID-BF4CEE45-BB6F-443B-A588-A40D9BCE378F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BF4CEE45-BB6F-443B-A588-A40D9BCE378F.htm"
generated: "2025-11-28T19:06:38.719581Z"
description: Prints an expression to the command line or writes an expression to an open file
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

# prin1 (AutoLISP)

> Prints an expression to the command line or writes an expression to an open file

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BF4CEE45-BB6F-443B-A588-A40D9BCE378F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-BF4CEE45-BB6F-443B-A588-A40D9BCE378F.htm)
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
(prin1
[expr [file-desc]]
)
```

- ***expr*:** **Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil  A string or AutoLISP expression. Only the specified *expr*  is printed; no newline or space is included.
- ***file-desc*:** **Type:**  File or nil  A file descriptor for a file opened for writing.

## Return Values

**Type:**  Integer, Real, String, List, Symbol, File, Ename (entity name), T, or nil

The value of the evaluated *expr*. If called with no arguments, `prin1`  returns a null symbol.

Used as the last expression in a function, `prin1`  without arguments prints a blank line when the function completes, allowing the function to exit “quietly.”

## Remarks

If *expr*  is a string containing control characters, `prin1`  expands these characters with a leading \, as shown in the following table:

| Control codes |  |
| --- | --- |
| Code | Description |
| `\\` | \ character |
| `\"` | " character |
| `\e` | Escape character |
| `\n` | Newline character |
| `\r` | Return character |
| `\t` | Tab character |
| `\`  *nnn* | Character whose octal code is *nnn* |

The following shows how to use control characters:

```lisp
(prin1 (chr 2))

"\002""\002"
```

## Examples

```lisp
(setq a 123 b '(a))

(A)

(prin1 'a)

AA
```

The previous command printed A and returned A.

```lisp
(prin1 a)

123123
```

The previous command printed 123 and returned 123.

```lisp
(prin1 b)

(A)(A)
```

The previous command printed (A) and returned (A).

Each preceding example is displayed on the screen because no *file-desc*  was specified. Assuming that `f`  is a valid file descriptor for a file opened for writing, the following function call writes a string to that file and returns the string:

```lisp
(prin1 "Hello" f)

"Hello"
```
