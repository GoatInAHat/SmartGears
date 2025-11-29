---
title: Passing Parameters to Functions
guid: "GUID-E204B037-D6DF-4DDC-9061-E1A2F6E1FA62"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E204B037-D6DF-4DDC-9061-E1A2F6E1FA62.htm"
generated: "2025-11-28T19:06:58.361667Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 03/12/2019
topic_subtype:
  - autolisp
---

# Passing Parameters to Functions

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E204B037-D6DF-4DDC-9061-E1A2F6E1FA62.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-E204B037-D6DF-4DDC-9061-E1A2F6E1FA62.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 03/12/2019

A better way to convey information from one function to another is to pass parameters to the called function. Design the function so it expects to receive a number of values. Remember the `Degrees->Radians`  function? This function is passed a parameter named `numberOfDegrees`:

```lisp
(defun Degrees->Radians (numberOfDegrees)
(* pi (/ numberOfDegrees 180.0)))
```

When you call the function, it expects you to pass it a number. The number within `Degrees->Radians`  is declared as the parameter named `numberOfDegrees`. For example:

```lisp
(degrees->radians 90)

1.5708
```

In this case, the number 90 is assigned to the parameter `numberOfDegrees`.

You can also pass a variable to a function. For example, you might have a variable called `aDegreeValue`  that contains the number 90. The following commands set `aDegreeValue`  and pass the variable to `Degrees->Radians`:

```lisp
(setq aDegreeValue 90)

90

(degrees->radians aDegreeValue)

1.5708
```
