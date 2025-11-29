---
title: About Referencing Variables in Document Namespaces (AutoLISP)
guid: "GUID-914D27BD-60F2-47E6-BF28-310AD365F783"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-914D27BD-60F2-47E6-BF28-310AD365F783.htm"
generated: "2025-11-28T19:05:58.371099Z"
description: "Variables defined in a separate-namespace VLX are not known to the document namespace associated with the VLX."
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

# About Referencing Variables in Document Namespaces (AutoLISP)

> Variables defined in a separate-namespace VLX are not known to the document namespace associated with the VLX.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-914D27BD-60F2-47E6-BF28-310AD365F783.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-914D27BD-60F2-47E6-BF28-310AD365F783.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

However, a separate-namespace VLX can access variables defined in a document namespace using the `vl-doc-ref`  and `vl-doc-set`  functions. The `vl-doc-set`  function is the same as using the `setq`  function.

The `vl-doc-ref`  function copies the value of a variable from a document namespace. The function requires a single argument, a symbol identifying the variable to be copied. For example, the following copies the value of a variable named `aruhu`:

```lisp
(vl-doc-ref 'aruhu)
```

If executed within a document namespace, `vl-doc-ref`  is equivalent to the `eval`  function.

The `vl-doc-set`  function sets the value of a variable in a document namespace. The function requires two arguments: a symbol identifying the variable to be set, and the value to set for the variable.

For example, the following sets the value of a variable named `ulus`:

```lisp
(vl-doc-set 'ulus "Go boldly to noone")
```

If executed within a document namespace, `vl-doc-set`  is equivalent to the `setq`  function. Use the `vl-propagate`  function to set the value of a variable in all open document namespaces.

For example, the following sets a variable named `fooyall`  in all open document namespaces:

```lisp
(setq fooyall "Go boldly and carry a soft stick")
(vl-propagate 'fooyall)
```

The `vl-propagate`  function not only copies the value of `fooyall`  into all currently open document namespaces, but also causes `fooyall`  to automatically be copied to the namespace of any new drawings opened during the current AutoCAD session.
