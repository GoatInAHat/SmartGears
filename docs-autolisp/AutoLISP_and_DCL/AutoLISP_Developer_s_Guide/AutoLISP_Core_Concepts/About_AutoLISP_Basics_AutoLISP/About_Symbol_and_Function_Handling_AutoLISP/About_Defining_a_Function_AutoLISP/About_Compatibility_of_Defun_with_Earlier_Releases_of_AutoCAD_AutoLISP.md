---
title: About Compatibility of Defun with Earlier Releases of AutoCAD (AutoLISP)
guid: "GUID-16246E02-459F-4408-9CB4-6F2CB306FD9F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-16246E02-459F-4408-9CB4-6F2CB306FD9F.htm"
generated: "2025-11-28T19:06:04.368919Z"
description: The internal implementation of defun changed in AutoCAD 2000.
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

# About Compatibility of Defun with Earlier Releases of AutoCAD (AutoLISP)

> The internal implementation of defun changed in AutoCAD 2000.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-16246E02-459F-4408-9CB4-6F2CB306FD9F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-16246E02-459F-4408-9CB4-6F2CB306FD9F.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

This change is transparent to the great majority of AutoLISP users upgrading from earlier AutoCAD releases. The change only affects AutoLISP code that manipulated `defun`  definitions as a list structure, such as by appending one function to another, as in the following code:

```lisp
(append s::startup (cdr mystartup))
```

For situations like this, you can use `defun-q`  to define your functions. An attempt to use a `defun`  function as a list results in an error. The following example illustrates the error:

```lisp
(defun foo (x) 4)

foo

(append foo '(3 4))

; error: Invalid attempt to access a compiled function definition.
You may want to define it using defun-q: #<SUBR @024bda3c FOO>
```

The error message alerts you to the possibility of using `defun-q`  instead of `defun`. The `defun-q`  function is provided strictly for backward compatibility with earlier releases and should not be used for other purposes.
