---
title: About Integers (AutoLISP)
guid: "GUID-EF6114FC-F1E4-4C71-91CC-07D01E6C8ABB"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EF6114FC-F1E4-4C71-91CC-07D01E6C8ABB.htm"
generated: "2025-11-28T19:05:59.966369Z"
description: Integers are whole numbers; numbers that do not contain a decimal point.
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

# About Integers (AutoLISP)

> Integers are whole numbers; numbers that do not contain a decimal point.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EF6114FC-F1E4-4C71-91CC-07D01E6C8ABB.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-EF6114FC-F1E4-4C71-91CC-07D01E6C8ABB.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 03/12/2019

AutoLISP integers are 32-bit signed numbers with values ranging from +2,147,483,647 to -2,147,483,648. Some functions through, only accept 16-bit numbers ranging from +32767 to -32678. When you explicitly use an integer, that value is known as a constant. Numbers such as 2, -56, and 1,200,196 are valid integers.

If you enter a number that is greater than the maximum integer allowed (resulting in integer overflow), AutoLISP converts the integer to a real number. However, if you perform an arithmetic operation on two valid integers, and the result is greater than the maximum allowable integer, the resulting number will be invalid.

The following examples demonstrate how AutoLISP handles integer overflow.

The largest positive integer value retains its specified value:

```lisp
(setq int1 2147483647)

2147483647
```

If you enter an integer that is greater than the largest allowable value, AutoLISP returns the value as a real:

```lisp
(setq int2 2147483648)

2.14748e+009
```

An arithmetic operation involving two valid integers, but resulting in integer overflow, produces an invalid result:

```lisp
(setq int3 (+ 2147483646 3))

-2147483647
```

In the previous example the result is clearly invalid, as the addition of two positive numbers results in a negative number. But note how the following operation produces a valid result:

```lisp
(setq int4 (+ 2147483648 2))

2.14748e+009
```

In this instance, AutoLISP converts 2147483648 to a valid real before adding 2 to the number. The result is a valid real. The largest negative integer value retains its specified value:

```lisp
(setq int5 -2147483647)

-2147483647
```

If you enter a negative integer larger than the greatest allowable negative value, AutoLISP returns the value as a real:

```lisp
(setq int6 -2147483648)

-2.14748e+009
```

The following operation concludes successfully, because AutoLISP first converts the overflow negative integer to a valid real:

```lisp
(setq int7 (- -2147483648 1))

-2.14748e+009
```
