---
title: "vl-get-resource (AutoLISP/Visual LISP IDE)"
guid: "GUID-947B44CE-EAC4-4B17-83E1-8521AD5F5341"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-947B44CE-EAC4-4B17-83E1-8521AD5F5341.htm"
generated: "2025-11-28T19:06:54.172547Z"
description: Returns the text stored in a .txt file packaged in a VLX
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

# vl-get-resource (AutoLISP/Visual LISP IDE)

> Returns the text stored in a . txt file packaged in a VLX

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-947B44CE-EAC4-4B17-83E1-8521AD5F5341.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-947B44CE-EAC4-4B17-83E1-8521AD5F5341.htm)
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
(vl-get-resource
text-file
)
```

- ***text-file*:** **Type:**  String  Name of a *.txt*  file packaged with the VLX. Do not include the *.txt*  extension when specifying the file name.

## Return Values

**Type:**  String

A textual value containing the text in *text-file*.

## Remarks

VLX files are supported on Windows only.

Note:
 This function is supported on Mac OS and Web, but does not affect the program.

## Examples

Assume the *getres.vlx*  file contains a LISP program defining a function named `print-readme`, and a text file named *readme.txt*. The `print-readme`  function is defined as follows:

```lisp
(defun print-readme ()
   (princ (vl-get-resource "readme"))
   (princ)
)
```

After loading *getres.vlx*, invoke `print-readme`:

```lisp
(print-readme)

Product Readme text
Product Readme text 2
```
