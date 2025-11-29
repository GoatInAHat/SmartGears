---
title: getkword (AutoLISP)
guid: "GUID-9F940144-0D7B-4DA1-BF50-BBF8FB8DFF21"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9F940144-0D7B-4DA1-BF50-BBF8FB8DFF21.htm"
generated: "2025-11-28T19:06:31.128076Z"
description: Pauses for user input of a keyword, and returns that keyword
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

# getkword (AutoLISP)

> Pauses for user input of a keyword, and returns that keyword

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9F940144-0D7B-4DA1-BF50-BBF8FB8DFF21.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-9F940144-0D7B-4DA1-BF50-BBF8FB8DFF21.htm)
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
(getkword
[msg]
)
```

- ***msg*:** **Type:**  String  Message to be displayed to prompt the user; if omitted, `getkword`  does not display a prompting message.

## Return Values

**Type:**  String or nil

A string representing the keyword entered by the user; otherwise `nil`, if the user presses Enter without typing a keyword. The function also returns `nil`  if it was not preceded by a call to `initget`  to establish one or more keywords.

If the user enters a value that is not a valid keyword, `getkword`  displays a warning message and prompts the user to try again.

## Remarks

Valid keywords are set prior to the `getkword`  call with the `initget`  function. The user cannot enter another AutoLISP expression as the response to a `getkword`  request.

## Examples

The following example shows an initial call to `initget`  that sets up a list of keywords (Yes and No) and disallows null input (*bits*  value equal to 1) to the `getkword`  call that follows:

```lisp
(initget 1 "Yes No")

nil

(setq x (getkword "Are you sure? (Yes or No) "))

Are you sure? (Yes or No)
yes

"Yes"
```

The following sequence illustrates what happens if the user enters invalid data in response to `getkword`:

```lisp
(initget 1 "Yes No")

nil

(setq x (getkword "Are you sure? (Yes or No) "))

Are you sure? (Yes or No)
Maybe

Invalid option keyword.
Are you sure? (Yes or No)
yes

"Yes"
```

The user's response was not one of the keywords defined by the preceding `initget`, so `getkword`  issued an error message and then prompted the user again with the string supplied in the *msg*  argument.
