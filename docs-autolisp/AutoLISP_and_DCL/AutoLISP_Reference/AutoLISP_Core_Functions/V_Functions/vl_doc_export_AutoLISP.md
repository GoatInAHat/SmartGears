---
title: "vl-doc-export (AutoLISP)"
guid: "GUID-696970BC-3669-412C-8194-7ADD950EE7BA"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-696970BC-3669-412C-8194-7ADD950EE7BA.htm"
generated: "2025-11-28T19:06:46.067446Z"
description: Makes a function available to the current document
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

# vl-doc-export (AutoLISP)

> Makes a function available to the current document

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-696970BC-3669-412C-8194-7ADD950EE7BA.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-696970BC-3669-412C-8194-7ADD950EE7BA.htm)
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
(vl-doc-export
'function
)
```

- ***'function*:** **Type:**  Subroutine  A symbol naming the function to be exported.

## Return Values

**Type:**  T

Always returns `T`.

## Remarks

When issued from a VLX that runs in its own namespace, `vl-doc-export`  exposes the specified function to any document namespace that loads the VLX.

The `vl-doc-export`  function should be used only at the top level in a file, and never inside other forms (for example, not within a `defun`).

Note:
 While the function is supported on Mac OS and Web, VLX files are not supported on Mac OS and Web which results in different behavior than when used on Windows.

## Examples

The following code shows the contents of a file named *kertrats.lsp*. This file is compiled into a VLX that runs in its own namespace. The VLX file is named *kertrats.vlx*. The `vl-doc-export`  call makes the `kertrats`  function visible to any document that loads *kertrats.vlx*:

```lisp
(vl-doc-export 'kertrats)
(defun kertrats ()
  (princ "This function goes nowhere")
)
```
