---
title: "defun-q-list-set (AutoLISP)"
guid: "GUID-CDA30337-9889-4D33-9461-503F307D1360"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CDA30337-9889-4D33-9461-503F307D1360.htm"
generated: "2025-11-28T19:06:26.544428Z"
description: Sets the value of a symbol to be a function defined by a list
topic_type: "reference-adsk"
audience: programmer
experience_level: general
product: OARX
release: 2025
book: Developer
component: "AutoCAD-AutoLISP-Reference"
created: 21/10/2024
modified: 29/03/2023
topic_subtype:
  - autolisp
  - function
---

# defun-q-list-set (AutoLISP)

> Sets the value of a symbol to be a function defined by a list

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CDA30337-9889-4D33-9461-503F307D1360.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-CDA30337-9889-4D33-9461-503F307D1360.htm)
- Topic Type: reference-adsk
- Subtypes: autolisp, function
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP-Reference
- Timeline: Created 21/10/2024, Updated 29/03/2023

**Supported Platforms:**  Windows, Mac OS, and Web

## Signature

```lisp
(defun-q-list-set
'sym list
)
```

- ***sym*:** **Type:**  Symbol  A symbol naming the function
- ***list*:** **Type:**  List  A list containing the expressions to be included in the function.

## Return Values

**Type:**  List, Symbol, or nil

The *sym*  defined.

## Examples

```lisp
(defun-q-list-set 'foo '((x) x))

FOO

(foo 3)

3
```

The following example illustrates the use of `defun-q-list-set`  to combine two functions into a single function with `defun-q`:

```lisp
(defun-q s::startup (x) (print x))

S::STARTUP

(defun-q my-startup (x) (print (list x)))

MY-STARTUP
```

Use `defun-q-list-set`  to combine the functions into a single function:

```lisp
(defun-q-list-set 's::startup (append
   (defun-q-list-ref 's::startup)
   (cdr (defun-q-list-ref 'my-startup))))

S::STARTUP
```

The following illustrates how the functions respond individually, and how the functions work after being combined using `defun-q-list-set`:

```lisp
(defun-q foo (x) (print (list 'foo x)))

FOO

(foo 1)

(FOO 1) (FOO 1)

(defun-q bar (x) (print (list 'bar x)))

BAR

(bar 2)

(BAR 2) (BAR 2)

(defun-q-list-set
  'foo
  (append (defun-q-list-ref 'foo)
              (cdr (defun-q-list-ref 'bar))
  ))

FOO

(foo 3)

(FOO 3)
(BAR 3) (BAR 3)
```
