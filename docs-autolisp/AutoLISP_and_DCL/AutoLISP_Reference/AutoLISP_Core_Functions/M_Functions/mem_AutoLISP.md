---
title: mem (AutoLISP)
guid: "GUID-F4AEB953-2117-4BF2-8056-EA1384AC3FFF"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F4AEB953-2117-4BF2-8056-EA1384AC3FFF.htm"
generated: "2025-11-28T19:06:36.769073Z"
description: Displays the current state of the AutoLISP memory
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

# mem (AutoLISP)

> Displays the current state of the AutoLISP memory

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F4AEB953-2117-4BF2-8056-EA1384AC3FFF.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-F4AEB953-2117-4BF2-8056-EA1384AC3FFF.htm)
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
(mem)
```

No arguments.

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

The `mem`  function displays statistics on AutoLISP memory usage. The first line of this statistics report contains the following information:

- **GC calls:** Number of garbage collection calls since AutoLISP started.
- **GC run time:** Total time spent collecting garbage (in milliseconds).

LISP objects are allocated in dynamic (heap) memory that is organized in segments and divided into pages.

- **PgSz:** Dynamic memory page size (in KB).
- **Used:** Number of pages used.
- **Free:** Number of free (empty) pages.
- **FMCL:** Largest contiguous area of free pages.
- **Segs:** Number of segments allocated.
- **Type:** Internal description of the types of objects allocated in this segment. These include  lisp stacks—LISP internal stacks  bytecode area—compiled code function modules  CONS memory—CONS objects  ::new—untyped memory requests served using this segment  DM Str—dynamic string bodies  DMxx memory—all other LISP nodes  bstack body—internal structure used for IO operations

The final line in the report lists the minimal segment size and the number of allocated segments. AutoLISP keeps a list of no more than three free segments in order to save system calls for memory requests.

All heap memory is global; that is, all AutoCAD documents share the same heap. This could change in future releases of AutoCAD.

Note that `mem`  does not list all memory requested from the operating system; it lists only those requests served by the AutoLISP Dynamic Memory (DM) subsystem. Some AutoLISP classes do not use DM for memory allocation.

## Examples

```lisp
(mem)

; GC calls: 23; GC run time: 298 ms
Dynamic memory segments statistic:
PgSz  Used  Free  FMCL  Segs  Type
 512    79    48    48     1  lisp stacks
 256  3706   423   142    16  bytecode area
4096   320    10    10    22  CONS memory
  32   769  1213  1089     1  ::new
4096   168    12    10    12  DM Str
4096   222     4     4    15  DMxx memory
 128     4   507   507     1  bstack body
Segment size: 65536, total used: 68, free: 0
nil
```
