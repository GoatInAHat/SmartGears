---
title: snvalid (AutoLISP)
guid: "GUID-2EFBE198-9860-456B-A090-206C743ACB90"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2EFBE198-9860-456B-A090-206C743ACB90.htm"
generated: "2025-11-28T19:06:41.485446Z"
description: Checks the symbol table name for valid characters
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

# snvalid (AutoLISP)

> Checks the symbol table name for valid characters

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2EFBE198-9860-456B-A090-206C743ACB90.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-2EFBE198-9860-456B-A090-206C743ACB90.htm)
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
(snvalid
sym_name [flag]
)
```

- ***sym_name*:** **Type:**  String  Symbol table name.
- ***flag*:** **Type:**  Integer  Specifies whether the vertical bar character is allowed within *sym_name*. The *flag*  argument can be one of the following:  **0**  -- Do not allow vertical bar characters anywhere in *sym_name*. This is the default.  **1**  -- Allow vertical bar characters in *sym_name*, as long as they are not the first or last characters in the name.

## Return Values

**Type:**  T or nil

`T`, if *sym_name*  is a valid symbol table name; otherwise `nil`.

## Remarks

The `snvalid`  function inspects the AutoCAD EXTNAMES system variable to determine the rules to enforce for the active drawing. If EXTNAMES is 0, `snvalid`  validates using the symbol name rules in effect prior to AutoCAD 2000. If EXTNAMES is 1 (the default value), `snvalid`  validates using the rules for extended symbol names introduced with AutoCAD 2000. The following are not allowed in symbol names, regardless of the setting of EXTNAMES:

- Control and graphic characters
- Null strings
- Vertical bars as the first or last character of the name

AutoLISP does not enforce restrictions on the length of symbol table names if EXTNAMES is 1.

If EXTNAMES is 1, all characters are allowed except control and graphic characters and the following:

| Characters disallowed in symbol table names |  |
| --- | --- |
| < > | less-than and greater-than symbol |
| / \ | forward slash and backslash |
| " | quotation mark |
| : | colon |
| ? | question mark |
| * | asterisk |
| | | vertical bar |
| , | comma |
| = | equal sign |
| ` | backquote |
| ; | semicolon (ASCII 59) |

A symbol table name may contain spaces.

If EXTNAMES is 0, symbol table names can consist of uppercase and lowercase alphabetic letters (e.g., A-Z), numeric digits (e.g., 0-9), and the dollar sign ($), underscore (_), and hyphen (-) characters.

## Examples

The following examples assume EXTNAMES is set to 1:

```lisp
(snvalid "hocus-pocus")

T

(snvalid "hocus pocus")

T

(snvalid "hocus%pocus")

T
```

The following examples assume EXTNAMES is set to 0:

```lisp
(snvalid "hocus-pocus")

T

(snvalid "hocus pocus")

nil

(snvalid "hocus%pocus")

nil
```

The following example includes a vertical bar in the symbol table name:

```lisp
(snvalid "hocus|pocus")

nil
```

By default, the vertical bar character is considered invalid in all symbol table names.

In the following example, the *flag*  argument is set to 1, so `snvalid`  considers the vertical bar character to be valid in *sym_name*, as long as it is not the first or last character in the name:

```lisp
(snvalid "hocus|pocus" 1)

T
```
