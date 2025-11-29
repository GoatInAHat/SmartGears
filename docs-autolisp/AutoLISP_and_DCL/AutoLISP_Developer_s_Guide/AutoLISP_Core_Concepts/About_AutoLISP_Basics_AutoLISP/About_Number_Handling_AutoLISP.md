---
title: About Number Handling (AutoLISP)
guid: "GUID-A5710494-E1A4-4C56-8E28-DD376792E47B"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A5710494-E1A4-4C56-8E28-DD376792E47B.htm"
generated: "2025-11-28T19:06:02.883159Z"
description: AutoLISP provides functions for working with integers and real numbers.
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

# About Number Handling (AutoLISP)

> AutoLISP provides functions for working with integers and real numbers.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A5710494-E1A4-4C56-8E28-DD376792E47B.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-A5710494-E1A4-4C56-8E28-DD376792E47B.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

In addition to performing basic and complex mathematical computations, you can use the number-handling functions to help you in your daily use of AutoCAD. If you are drawing a steel connection detail that uses a 2.5" bolt with a diameter of 0.5" and has 13 threads per inch, you could calculate the total number of threads for the bolt by multiplying 2.5 by 13. In AutoLISP, this is done with the multiplication (**`*`**) function. Remember, the name of a function comes before the arguments you are passing to a function.

```lisp
(* 2.5 13)

32.5
```

The arithmetic functions that have a *number*  argument (as opposed to *num*  or *angle*, for example) return different values if you provide integers or reals as arguments. If all arguments are integers, the value returned is an integer. However, if one or all the arguments are reals, the value returned is a real.

```lisp
(/ 12 5)

2

(/ 12.0 5)

2.4
```
