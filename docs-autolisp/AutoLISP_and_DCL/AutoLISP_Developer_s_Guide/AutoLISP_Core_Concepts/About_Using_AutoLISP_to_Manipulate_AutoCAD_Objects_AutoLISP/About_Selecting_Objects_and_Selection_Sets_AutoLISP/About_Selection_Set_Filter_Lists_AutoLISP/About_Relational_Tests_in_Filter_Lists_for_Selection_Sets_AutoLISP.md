---
title: About Relational Tests in Filter Lists for Selection Sets (AutoLISP)
guid: "GUID-877CEEA5-0D88-48DF-8450-B9421031A1B7"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-877CEEA5-0D88-48DF-8450-B9421031A1B7.htm"
generated: "2025-11-28T19:06:11.225624Z"
description: "Unless otherwise specified, an equivalency is implied for each item in the filter-list."
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

# About Relational Tests in Filter Lists for Selection Sets (AutoLISP)

> Unless otherwise specified, an equivalency is implied for each item in the filter-list .

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-877CEEA5-0D88-48DF-8450-B9421031A1B7.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-877CEEA5-0D88-48DF-8450-B9421031A1B7.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

For numeric group codes (integers, reals, points, and vectors), you can specify other relations by including a special -4 group code that specifies a relational operator. The value of a -4 group code is a string indicating the test operator to be applied to the next group in the *filter-list*.

The following selects all circles with a radius (group code 40) greater than or equal to 2.0:

```lisp
(ssget "X" '((0 . "CIRCLE") (-4 . ">=") (40 . 2.0)))
```

The possible relational operators are shown in the following table:

| Relational operators for selection set filter lists |  |
| --- | --- |
| Operator | Description |
| `"*"` | Anything goes (always true) |
| `"="` | Equals |
| `"!="` | Not equal to |
| `"/="` | Not equal to |
| `"<>"` | Not equal to |
| `"<"` | Less than |
| `"<="` | Less than or equal to |
| `">"` | Greater than |
| `">="` | Greater than or equal to |
| `"&"` | Bitwise `AND`  (integer groups only) |
| `"&="` | Bitwise masked equals (integer groups only) |

The use of relational operators depends on the kind of group code value you are testing:

- All relational operators except for the bitwise operators (
  "&"
   and
  "&="
  ) are valid for both real- and integer-valued groups.
- The bitwise operators
  "&"
   and
  "&="
   are valid only for integer-valued groups.

  The bitwise `AND`, `"&"`, is true if `((*integer_group*  & *filter*) /= 0)` —that is, if any of the bits set in the mask are also set in *integer_group*.

  The bitwise masked equals, `"&="`, is true if `((*integer_group*  & *filter*) = filter)` —that is, if all bits set in the mask are also set in *integer_group*  (other bits might be set in the *integer_group*  but are not checked).
- For point group codes, the
  X
  ,
  Y
  , and
  Z
   tests can be combined into a single string, with each operator separated by commas (for example,
   ">,>,*"
  ). If an operator is omitted from the string (for example,
  "=,<>"
   leaves out the
  Z
  test), then the “anything goes” operator,
  "*"
  , is assumed.
- Direction vectors (group code 210) can be compared only with the operators
  "*"
  ,
  "="
  , and
  "!="
   (or one of the equivalent “not equal” strings).
- You cannot use the relational operators with string group codes; use wild-card tests instead.
