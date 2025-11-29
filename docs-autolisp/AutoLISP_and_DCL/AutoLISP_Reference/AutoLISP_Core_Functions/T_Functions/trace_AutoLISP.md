---
title: trace (AutoLISP)
guid: "GUID-87EEF5AD-2250-401D-B722-0CD122DBAE70"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-87EEF5AD-2250-401D-B722-0CD122DBAE70.htm"
generated: "2025-11-28T19:06:44.286104Z"
description: Aids in AutoLISP debugging
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

# trace (AutoLISP)

> Aids in AutoLISP debugging

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-87EEF5AD-2250-401D-B722-0CD122DBAE70.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-87EEF5AD-2250-401D-B722-0CD122DBAE70.htm)
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
(trace
[function ...]
)
```

- ***function*:** **Type:**  Symbol  A symbol that names a function. If no argument is supplied, `trace`  has no effect.

## Return Values

**Type:**  Symbol or nil

The last function name passed to `trace`. If no argument is supplied, `trace`  returns `nil`.

## Remarks

The `trace`  function sets the trace flag for the specified functions. Each time a specified function is evaluated, a trace display appears showing the entry of the function (indented to the level of calling depth) and prints the result of the function.

Trace output is sent one of the following locations

- AutoCAD Command Line window; when Visual LISP is not active in AutoCAD on Windows
- Visual LISP Trace window (Not available in AutoCAD LT for Windows, or on Mac OS and Web)

Note:
 Once you start Visual LISP during an AutoCAD session, it remains active until you exit AutoCAD. Therefore, all
trace
 output prints in the Visual LISP Trace window for the remainder of that AutoCAD session. Exiting or closing Visual LISP while AutoCAD is running only closes the IDE windows and places Visual LISP in a quiescent state; it does not result in a true shutdown. You must reopen Visual LISP to view the output in the Trace window.

Use `untrace`  to turn off the trace flag.

## Examples

Define a function named `foo`  and set the trace flag for the function:

```lisp
(defun foo (x) (if (> x 0) (foo (1- x))))

FOO

(trace foo)

FOO
```

Invoke `foo`  and observe the results:

```lisp
(foo 3)

Entering (FOO 3)
Entering (FOO 2)
Entering (FOO 1)
Entering (FOO 0)
Result: nil
Result: nil
Result: nil
Result: nil
```

Clear the trace flag by invoking `untrace`:

```lisp
(untrace foo)

FOO
```
