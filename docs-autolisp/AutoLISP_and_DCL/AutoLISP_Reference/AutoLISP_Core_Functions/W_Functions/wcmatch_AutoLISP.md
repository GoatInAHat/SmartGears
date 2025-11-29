---
title: wcmatch (AutoLISP)
guid: "GUID-EC257AF7-72D4-4B38-99B6-9B09952A53AD"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EC257AF7-72D4-4B38-99B6-9B09952A53AD.htm"
generated: "2025-11-28T19:06:52.237477Z"
description: "Performs a wild-card pattern match on a string"
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

# wcmatch (AutoLISP)

> Performs a wild-card pattern match on a string

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EC257AF7-72D4-4B38-99B6-9B09952A53AD.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EC257AF7-72D4-4B38-99B6-9B09952A53AD.htm)
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
(wcmatch
str pattern
)
```

- ***str*:** **Type:**  String  A textual value to be compared. The comparison is case-sensitive, so uppercase and lowercase characters must match.
- ***pattern*:** **Type:**  String  Pattern to match against *str*. The *pattern*  can contain the wild-card pattern-matching characters shown in the table Wild-card characters. You can use commas in a pattern to enter more than one pattern condition. Only the first 500 characters (approximately) of the *str*  and *pattern*  are compared; anything beyond that is ignored.

## Return Values

**Type:**  String or nil

If *str*  and *pattern*  match, `wcmatch`  returns `T`; otherwise, `wcmatch`  returns `nil`.

| Wild-card characters |  |
| --- | --- |
| Character | Definition |
| `#`  (pound) | Matches any single numeric digit. |
| `@`  (at) | Matches any single alphabetic character. |
| `.`  (period) | Matches any single nonalphanumeric character. |
| `*`  (asterisk) | Matches any character sequence, including an empty one, and it can be used anywhere in the search pattern: at the beginning, middle, or end. |
| `?`  (question mark) | Matches any single character. |
| `~`  (tilde) | If it is the first character in the pattern, it matches anything except the pattern. |
| `[...]` | Matches any one of the characters enclosed. |
| `[~...]` | Matches any single character not enclosed. |
| `-` (hyphen) | Used inside brackets to specify a range for a single character. |
| `,`  (comma) | Separates two patterns. |
| `````  (reverse quote) | Escapes special characters (reads next character literally). |

## Remarks

Both arguments can be either a quoted string or a string variable. It is valid to use variables and values returned from AutoLISP functions for *str*  and *pattern*  values.

To test for a wild-card character in a string, you can use the single reverse-quote character (`````) to *escape*  the character. *Escape*  means that the character following the single reverse quote is not read as a wild-card character; it is compared at its face value. For example, to search for a comma anywhere in the string “ `Name` ”, enter the following:

```lisp
(wcmatch "Name" "*`,*")

nil
```

Both the C and AutoLISP programming languages use the backslash (\) as an escape character, so you need two backslashes (\\) to produce one backslash in a string. To test for a backslash character anywhere in “ `Name` ”, use the following function call:

```lisp
(wcmatch "Name" "*`\\*")

nil
```

All characters enclosed in brackets (`[`  . . . `]`) are read literally, so there is no need to escape them, with the following exceptions: the tilde character (~) is read literally only when it is not the first bracketed character (as in `"[A~BC]"`); otherwise, it is read as the negation character, meaning that `wcmatch`  should match all characters except those following the tilde (as in `"[~ABC]"`). The dash character (-) is read literally only when it is the first or last bracketed character (as in `"[-ABC]"`  or `"[ABC-]"`) or when it follows a leading tilde (as in `"[~-ABC]"`). Otherwise, the dash character (-) is used within brackets to specify a range of values for a specific character. The range works only for single characters, so `"STR[1-38]"`  matches `STR1`, `STR2`, `STR3`, and `STR8`, and `"[A-Z]"`  matches any single uppercase letter.

The closing bracket character (`]`) is also read literally if it is the first bracketed character or if it follows a leading tilde (as in `"[ ]ABC]"`  or `"[~]ABC]"`).

Note:
 Because additional wild-card characters might be added in future releases of AutoLISP, it is a good idea to escape all nonalphanumeric characters in your pattern to ensure upward compatibility.

## Examples

Examples

The following command tests a string to see if it begins with the character `N:`

```lisp
(wcmatch "Name" "N*")

T
```

The following example performs three comparisons. If any of the three pattern conditions is met, `wcmatch`  returns `T`. The tests are:

- Does the string contain three characters?
- Does the string not contain an
  m
  ?
- Does the string begin with the letter “
  N
  ”?

If any of the three pattern conditions is met, `wcmatch`  returns `T`:

```lisp
(wcmatch "Name" "???,~*m*,N*")

T
```

In this example, the last condition was met, so `wcmatch`  returned `T`.
