---
title: About Catching Errors and Continuing Program Execution (AutoLISP)
guid: "GUID-9DDAD8C4-FB10-4C15-B59E-F6E05C1F9672"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9DDAD8C4-FB10-4C15-B59E-F6E05C1F9672.htm"
generated: "2025-11-28T19:06:05.898820Z"
description: Programs should intercept and attempt to process errors instead of allowing control to pass to *error* when possible.
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

# About Catching Errors and Continuing Program Execution (AutoLISP)

> Programs should intercept and attempt to process errors instead of allowing control to pass to *error* when possible.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9DDAD8C4-FB10-4C15-B59E-F6E05C1F9672.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-9DDAD8C4-FB10-4C15-B59E-F6E05C1F9672.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The `vl-catch-all-apply`  function is designed to invoke any function, return a value from the function, and trap any error that may occur. The function requires two arguments:

- A symbol identifying a function or lambda expression
- A list of arguments to be passed to the called function

The importance of `vl-catch-all-apply`  is the ability to catch errors and allow your program to continue execution. The following example uses `vl-catch-all-apply`  to divide two numbers:

```lisp
(setq catchit (vl-catch-all-apply '/ '(50 5)))

10
```

The result from this example is the same as if you had used apply to perform the division or just used **`/`**  to divide the provided numbers.

The following example uses `vl-catch-all-apply`  to divide two numbers, and one of the numbers being zero:

```lisp
(setq catchit (vl-catch-all-apply '/ '(5 0)))

#<%catch-all-apply-error%>
```

The result from this example returns a VL-CATCH-ALL-APPLY-ERROR object which can be interpreted using `vl-catch-all-error-message`. You can use the type function to make sure you are working with an error object before calling `vl-catch-all-error-message`.

The following example checks for an error object and returns the error message:

```lisp
(if (vl-catch-all-error-p catchit)
  (vl-catch-all-error-message catchit)
)

"divide by zero"
```
