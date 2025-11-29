---
title: type (AutoLISP)
guid: "GUID-506C9CC8-B0BD-4A4C-B4C2-006750504509"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-506C9CC8-B0BD-4A4C-B4C2-006750504509.htm"
generated: "2025-11-28T19:06:44.470252Z"
description: Returns the type of a specified item
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

# type (AutoLISP)

> Returns the type of a specified item

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-506C9CC8-B0BD-4A4C-B4C2-006750504509.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-506C9CC8-B0BD-4A4C-B4C2-006750504509.htm)
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
(type
item
)
```

- ***item*:** **Type:**  Varies  A symbol.

## Return Values

**Type:**  Varies or nil

The data type of *item*. Items that evaluate to `nil`  (such as unassigned symbols) return `nil`. The data type is returned as one of the atoms listed in the following table:

| Data types returned by the type function |  |
| --- | --- |
| Data type | Description |
| ENAME | Entity names |
| EXRXSUBR | External ObjectARX applications |
| FILE | File descriptors |
| INT | Integers |
| LIST | Lists |
| PAGETB | Function paging table |
| PICKSET | Selection sets |
| REAL | Floating-point numbers |
| SAFEARRAY | Safearray |
| STR | Strings |
| SUBR | Internal AutoLISP functions or functions loaded from compiled (FAS or VLX) files  Functions in LISP source files loaded from the AutoCAD Command prompt may also appear as SUBR |
| SYM | Symbols |
| VARIANT | Variant |
| USUBR | User-defined functions loaded from LISP source files |
| VLA-object | ActiveX objects |

Note:

- VLX files are supported on Windows only
- ActiveX objects and Safearrays are available in AutoCAD for Windows only; not available in AutoCAD LT for Windows

## Examples

For example, given the following assignments:

```lisp
(setq a 123 r 3.45 s "Hello!" x '(a b c))
(setq f (open "name" "r"))
```

then

```lisp
(type 'a)

returns
  SYM

(type a)

returns
  INT

(type f)

returns
  FILE

(type r)

returns
  REAL

(type s)

returns
  STR

(type x)

returns
  LIST

(type +)

returns
  SUBR

(type nil)

returns
  nil
```

The following code example uses the `type`  function on the argument passed to it:

```lisp
(defun isint (a)

(if (= (type a) 'INT)

is
 TYPE
integer?

T

yes, return
 T

nil

no, return
 nil

)

)
```
