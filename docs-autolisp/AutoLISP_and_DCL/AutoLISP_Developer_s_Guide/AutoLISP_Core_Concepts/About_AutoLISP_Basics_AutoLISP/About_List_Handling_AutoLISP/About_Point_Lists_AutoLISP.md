---
title: About Point Lists (AutoLISP)
guid: "GUID-BFED8707-AB01-49BC-B0A9-D872794311DF"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-BFED8707-AB01-49BC-B0A9-D872794311DF.htm"
generated: "2025-11-28T19:06:03.907054Z"
description: AutoLISP utilizes the list data type to represent graphical coordinate values.
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

# About Point Lists (AutoLISP)

> AutoLISP utilizes the list data type to represent graphical coordinate values.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-BFED8707-AB01-49BC-B0A9-D872794311DF.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-BFED8707-AB01-49BC-B0A9-D872794311DF.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

Points are expressed as *lists*  with either two or three numerical values.

- 2D point - List with two integer or real numbers, (X and Y, respectively), as in
  (3.4 7.52)
  .
- 3D point – List with three integer or real numbers, (X, Y, and Z, respectively), as in
  (3.4 7.52 1.0)
  .

You can use the `list`  function to form point lists, as shown in the following examples:

```lisp
(list 3.875 1.23)

(3.875 1.23)

(list 88.0 14.77 3.14)

(88.0 14.77 3.14)
```

To assign particular coordinates to a point variable, you can use one of the following expressions:

```lisp
(setq pt1 (list 3.875 1.23))

(3.875 1.23)

(setq pt2 (list 88.0 14.77 3.14))

(88.0 14.77 3.14)

(setq abc 3.45)

3.45

(setq pt3 (list abc 1.23))

(3.45 1.23)
```

The latter uses the value of variable `abc`  as the *X*  component of the point list. If all members of a list are constant values, you can use the `quote`  function to explicitly define the list, rather than the `list`  function. The `quote`  function returns an expression without evaluation, as follows:

```lisp
(setq pt1 (quote (4.5 7.5)))

(4.5 7.5)
```

The single quotation mark (**`'`**) can be used as shorthand for the `quote`  function. The following code produces the same result as the preceding code.

```lisp
(setq pt1 '(4.5 7.5))

(4.5 7.5)
```

The quote and (‘) functions cannot be used to create a list using values that are stored in a variable. The following code does not return the excepted results:

```lisp
(setq abc 3.45)

3.45

(setq pt4 (quote abc 1.23))

; error: syntax error
```

## Retrieve the X, Y, and Z components of a point list

You can retrieve the *X*, *Y*, and *Z*  components of a point list using three additional built-in functions; `car`, `cadr`, and `caddr`. The following code examples show how to retrieve values from a 3D point list. The `pt`  variable is set to the point 1.5,3.2,2:

```lisp
(setq pt '(1.5 3.2 2.0))

(1.5 3.2 2.0)
```

The `car`  function returns the first member of a list. In this example, it returns the *X*  value of the point list to the `x_val`  variable.

```lisp
(setq x_val (car pt))

1.5
```

The `cadr`  function returns the second member of a list. In this example it returns the Y value of the point list to the `y_val`  variable.

```lisp
(setq y_val (cadr pt))

3.2
```

The `caddr`  function returns the third member of a list. In this example it returns the Z value of the point list to the `z_val`  variable.

```lisp
(setq z_val (caddr pt))

2.0
```

You can use the following code to define the lower-left and upper-right (`pt1`  and `pt2`) corners of a rectangle, as follows:

```lisp
(setq pt1 '(1.0 2.0) pt2 '(3.0 4.0))

(3.0 4.0)
```

You can use the `car`  and `cadr`  functions to set the `pt3`  variable to the upper-left corner of the rectangle, by extracting the *X*  component of `pt1`  and the *Y*  component of `pt2`, as follows:

```lisp
(setq pt3 (list (car pt1) (cadr pt2)))

(1.0 4.0)
```

The preceding statement sets `pt3`  equal to point `(1.0, 4.0)`.

AutoLISP supports combinations of the `car`  and `cdr`  functions up to four levels deep. Some examples of these functions are `caaaar`  and `cadr`. Each a represents *a*  call to `car`  and each *d*  represents a call to `cdr`. For example:

```lisp
(caar x)

is equivalent to
  (car (car x))

(cdar x)

is equivalent to
  (cdr (car x))

(cadar x)

is equivalent to
  (car (cdr (car x)))

(cadr x)

is equivalent to
  (car (cdr x))

(cddr x)

is equivalent to
  (cdr (cdr x))

(caddr x)

is equivalent to
  (car (cdr (cdr x)))
```
