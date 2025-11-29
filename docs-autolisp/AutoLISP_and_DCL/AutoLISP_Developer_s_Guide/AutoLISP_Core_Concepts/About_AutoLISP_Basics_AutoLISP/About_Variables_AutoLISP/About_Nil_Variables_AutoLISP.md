---
title: About Nil Variables (AutoLISP)
guid: "GUID-B585FB5D-15D2-4B70-8A1A-3F91B5396307"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-B585FB5D-15D2-4B70-8A1A-3F91B5396307.htm"
generated: "2025-11-28T19:06:02.632894Z"
description: A variable that has not been assigned a value has a default value of nil.
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

# About Nil Variables (AutoLISP)

> A variable that has not been assigned a value has a default value of nil .

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-B585FB5D-15D2-4B70-8A1A-3F91B5396307.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-B585FB5D-15D2-4B70-8A1A-3F91B5396307.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

This is different from blank, which is considered a character string, and different from 0, which is a number. So, in addition to checking a variable for its current value, you can test to determine if the variable has been assigned a value.

Each variable consumes a small amount of memory, so it is good programming practice to reuse variable names or set variables to `nil`  when their values are no longer needed. Setting a variable to `nil`  releases the memory used to store that variable's value. If you no longer need the `val`  variable, you can release its value from memory with the following expression:

```lisp
(setq val nil)

nil
```

Another efficient programming practice is to use local variables whenever possible.
