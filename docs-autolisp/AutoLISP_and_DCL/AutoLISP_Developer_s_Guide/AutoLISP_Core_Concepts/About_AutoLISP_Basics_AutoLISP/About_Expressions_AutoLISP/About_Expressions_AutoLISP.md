---
title: About Expressions (AutoLISP)
guid: "GUID-AD10051A-822B-4E94-A615-F2A074E878C5"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-AD10051A-822B-4E94-A615-F2A074E878C5.htm"
generated: "2025-11-28T19:05:59.366575Z"
description: An expression is the basic structure that is used when working with AutoLISP.
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
---

# About Expressions (AutoLISP)

> An expression is the basic structure that is used when working with AutoLISP.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-AD10051A-822B-4E94-A615-F2A074E878C5.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-AD10051A-822B-4E94-A615-F2A074E878C5.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 29/03/2023

AutoLISP expressions have the following form:

```lisp
(function
arguments
)
```

Each expression:

- Begins with an open (left) parenthesis.
- Consists of a function name and optional arguments for that function. Each argument can also be an expression.
- Ends with a close (right) parenthesis.
- Returns a value that can be used by a surrounding expression. The value of the last interpreted expression is returned to the calling expression.

For example, the following code example involves three functions:

```lisp
(fun1 (fun2
arguments
)(fun3
arguments
))
```

The first function, `fun1`, has two arguments, which in this example are expressions. The values returned by the expressions are used by `fun1`. The other functions, `fun2`  and `fun3`, each have one argument. AutoLISP evaluates the innermost expression first, working its way outward. For this example, expressions containing `fun2`  and `fun3`  are evaluated before `fun1`.

The following example shows the use of the **`*`**  (multiplication) function, which accepts one or more numbers as arguments:

```lisp
(* 2 27)

54
```

Because this code example has no surrounding expression, AutoLISP returns the result to the window from which you entered the code.

Expressions nested within other expressions return their result to the surrounding expression.

The following example uses the result from the **`+`**  (addition) function as one of the arguments for the **`*`**  (multiplication) function.

```lisp
(* 2 (+ 5 10))

30
```

In the previous example, `(+ 5 10)`  returns a value of 5. After the innermost expression is evaluated, the AutoLISP interpreter sees the following:

```lisp
(* 2 15)

30
```

## Entering AutoLISP Expressions

AutoLISP expressions can be entered directly at the AutoCAD Command prompt, loaded with an AutoLISP source (LSP) file, or evaluated with the Visual LISP Editor (AutoCAD for Windows only). When you type an open (left) parenthesis, you indicate to AutoCAD that the following text should be passed to the AutoLISP interpreter for evaluation.

Note:
 The Visual LISP Editor is not available for AutoCAD LT for Windows or on Mac OS.

If you enter the incorrect number of close (right) parentheses, AutoLISP displays the following prompt:

```lisp
(_>
```

The number of open parentheses in this prompt indicates how many levels of open parentheses remain unclosed. If this prompt appears, you must enter the required number of close parentheses for the expression to be evaluated.

```lisp
(* 2 (+ 5 10

((_>
) )

30
```

A common mistake is to omit the closing quotation mark (") in a text string, in which case the close parentheses are interpreted as part of the string and have no effect in resolving the open parentheses. To correct this condition, press Shift+Esc to cancel the function, then re-enter it correctly.
