---
title: About Logical Grouping of Selection Filter Tests (AutoLISP)
guid: "GUID-5CB54129-22A1-42B9-B97C-2D2F5597F90E"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-5CB54129-22A1-42B9-B97C-2D2F5597F90E.htm"
generated: "2025-11-28T19:06:11.311893Z"
description: You can define test groups with nested Boolean expressions to filter objects from a selection set created with ssget.
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

# About Logical Grouping of Selection Filter Tests (AutoLISP)

> You can define test groups with nested Boolean expressions to filter objects from a selection set created with ssget .

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-5CB54129-22A1-42B9-B97C-2D2F5597F90E.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-5CB54129-22A1-42B9-B97C-2D2F5597F90E.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The following table lists the grouping operators that you can use to filter selection sets:

| Grouping operators for selection set filter lists |  |  |
| --- | --- | --- |
| Starting operator | Encloses | Ending operator |
| "<AND" | One or more operands | "AND>" |
| "<OR" | One or more operands | "OR>" |
| "<XOR" | Two operands | "XOR>" |
| "<NOT" | One operand | "NOT>" |

The grouping operators are specified by -4 dxf group codes, like the relational operators. They are paired and must be balanced correctly in the filter list or the `ssget`  call will fail.

```lisp
(ssget "X"
  '(
    (-4 . "<OR")
      (-4 . "<AND")
        (0 . "CIRCLE")
        (40 . 1.0)
      (-4 . "AND>")
      (-4 . "<AND")
        (0 . "LINE")
        (8 . "ABC")
      (-4 . "AND>")
    (-4 . "OR>")
  )
)
```

This filter list allows the selection of all circles with a radius of 1.0 plus all lines on layer "ABC". The grouping operators are not case-sensitive; for example, you can specify `"and>"`, `"<or"`, instead of `"AND>"`, `"<OR"`. Grouping operators are not allowed within the -3 dxf group code. Multiple application names specified in a -3 dxf group code use an implied `AND`  operator. If you want to test for extended data using other grouping operators, specify separate -3 dxf group codes and group them as desired.

The following example code demonstrates how to select all circles having extended data for either application "APP1" or "APP2" but not both:

```lisp
(ssget "X"
  '((0 . "CIRCLE")
    (-4 . "<XOR")
      (-3 ("APP1"))
      (-3 ("APP2"))
    (-4 . "XOR>")
  )
)
```

You can simplify the coding of frequently used grouping operators by setting them equal to a symbol. The previous example could be rewritten as follows (notice that in this example you must explicitly quote each list):

```lisp
(setq <xor '(-4 . "<XOR")
         xor> '(-4 . "XOR>"))

(ssget "X"
  (list
    '(0 . "CIRCLE")
    <xor
    '(-3 ("APP1"))
    '(-3 ("APP2"))
    xor>
  )
)
```
