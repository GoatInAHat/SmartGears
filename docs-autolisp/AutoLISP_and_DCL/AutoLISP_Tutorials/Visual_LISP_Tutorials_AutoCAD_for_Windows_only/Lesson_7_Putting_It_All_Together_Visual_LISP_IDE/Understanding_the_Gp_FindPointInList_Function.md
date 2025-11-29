---
title: "Understanding the Gp:FindPointInList Function"
guid: "GUID-9E5E5FA3-7277-4245-8993-86E24F94991A"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9E5E5FA3-7277-4245-8993-86E24F94991A.htm"
generated: "2025-11-28T19:07:06.771899Z"
topic_type: concept
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Tutorials"
created: 25/4/2024
modified: 02/12/2019
topic_subtype:
  - autolisp
---

# Understanding the Gp:FindPointInList Function

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9E5E5FA3-7277-4245-8993-86E24F94991A.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Tutorials/files/GUID-9E5E5FA3-7277-4245-8993-86E24F94991A.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Tutorials
- Timeline: Created 25/4/2024, Updated 02/12/2019

The function header in the source code explains how `gp:FindPointInList`  transforms the information it works with. Like the previous function, `gp:FindMovedPoint`, this function uses LISP's list manipulation capabilities to perform the work. When operating with lists, you will often see the mapcar and lambda functions used together as they are here. At first, these are strange and confusing functions, with names that do not indicate what they do. Once you learn how to use them, however, you will find them to be two of the most powerful functions within the AutoLISP repertoire. What follows is a brief overview of `mapcar`  and `lambda`.

The `mapcar`  function applies (maps) an expression to every item in a list. For example, given a list of the integers 1, 2, 3, and 4, `mapcar`  can be used to apply the `1+`  function to add 1 to each number in the list:

```lisp
(mapcar '1+ '(1 2 3 4))

(2 3 4 5)
```

An initial definition for `mapcar`  is that it maps the function given in the first parameter to the successive items in the second parameter—the list. The resulting value from a `mapcar`  operation is the list transformed by whatever function or expression was applied to it. (Actually, `mapcar`  can do more than that, but for now this definition will suffice.)

In the supplied example, every value in the list `'(1 2 3 4)`  was passed to the `1+`  function. Essentially, `mapcar`  performed the following operations, assembling the resulting values in a list:

```lisp
(1+ 1)  -> 2
(1+ 2)  -> 3
(1+ 3)  -> 4
(1+ 4)  -> 5
```

Here is another example of `mapcar`, this time using the `null`  function to test whether or not the values in a list are null (not true) values:

```lisp
(mapcar 'null (list 1 (= 3 "3") nil "Steve"))

(nil T T nil)
```

What happened in this code was essentially the following:

```lisp
(null 1)    -> nil
(null (= 3 "3")   -> T
(null nil)   -> T
(null "Steve")   -> nil
```

You can use many existing AutoLISP functions within a `mapcar`. You can also use your own functions. For example, imagine you have just created a very powerful function named `equals2`:

```lisp
(defun equals2(num)(= num 2))

EQUALS2

(mapcar 'equals2  '(1 2 3 4))

(nil T nil nil)
```

Okay, so `equals2`  is not all that powerful. But it is in such cases that `lambda`  comes in handy. You can use `lambda`  in cases where you do not want or need to go through the overhead of defining a function. You will sometimes see `lambda`  defined as an anonymous function. For example, instead of defining a function called `equals2`, you could write a `lambda`  expression to perform the same operation without the overhead of the function definition:

```lisp
(mapcar '(lambda (num) (= num 2)) '(1 2 3 4))

(nil T nil nil)
```

What happened in the code was this:

```lisp
(= 1 2)	-> nil
(= 2 2)	-> T
(= 3 2)	-> nil
(= 4 2)	-> nil
```

With this knowledge, see if the `gp:FindPointInList`  function makes sense. Again, review the comments within the source code.
