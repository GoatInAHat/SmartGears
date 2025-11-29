---
title: "About Wild-Card Matching (AutoLISP)"
guid: "GUID-E8328DF6-43FF-47AA-8524-72B962A9D552"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-E8328DF6-43FF-47AA-8524-72B962A9D552.htm"
generated: "2025-11-28T19:06:03.503587Z"
description: "A string can be compared to a wild-card pattern with the wcmatch function."
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# About Wild-Card Matching (AutoLISP)

> A string can be compared to a wild-card pattern with the wcmatch function.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-E8328DF6-43FF-47AA-8524-72B962A9D552.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-E8328DF6-43FF-47AA-8524-72B962A9D552.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

This can be helpful when needing to build a dynamic selection set (in conjunction with `ssget`) or to retrieve extended entity data by application name (in conjunction with `entget`). The `wcmatch`  function compares a single string to a pattern. The function returns `T`  if the string matches the pattern, and `nil`  if it does not. The wild-card patterns are similar to the regular expressions used by many system and application programs.

The following rules apply to wild-card patterns:

- Alphabetic characters and numerals are treated literally in the pattern.
- Brackets can be used to specify optional characters or a range of letters or digits.
- A question mark (

  ?

  ) matches a single character.
- An asterisk (

  *

  ) matches a sequence of characters; and, certain other special characters have special meanings within the pattern. When you use the

  *

   character at the beginning and end of the search pattern, you can locate the desired portion anywhere in the string.

In the following examples, a string variable called `matchme`  has been declared and initialized:

```lisp
(setq matchme "this is a string - test1 test2 the end")

"this is a string - test1 test2 the end"
```

The following code checks whether or not `matchme`  begins with the four characters "this":

```lisp
(wcmatch matchme "this*")

T
```

The following code illustrates the use of brackets in the pattern. In this case, `wcmatch`  returns `T`  if `matchme`  contains "test4", "test5", "test6" (4-6), or "test9" (note the use of the **`*`**  character):

```lisp
(wcmatch matchme "*test[4-69]*")

nil
```

In this case, `wcmatch`  returns `nil`  because `matchme`  does not contain any of the strings indicated by the pattern. However, using the pattern "test[4-61]" does match the string because it contains “test1”.

```lisp
(wcmatch matchme "*test[4-61]*")

T
```

The pattern string can specify multiple patterns, separated by commas. The following code returns `T`  if `matchme`  equals "ABC", or if it begins with "XYZ", or if it ends with "end".

```lisp
(wcmatch matchme "ABC,XYZ*,*end")

T
```
