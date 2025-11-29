---
title: About Sharing Data Between Namespaces (AutoLISP)
guid: "GUID-0C8F8E36-7C10-45C4-9EF6-C284E56A8EA2"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0C8F8E36-7C10-45C4-9EF6-C284E56A8EA2.htm"
generated: "2025-11-28T19:05:58.893273Z"
description: A namespace called the blackboard is used for communicating values across all namespaces.
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

# About Sharing Data Between Namespaces (AutoLISP)

> A namespace called the blackboard is used for communicating values across all namespaces.

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0C8F8E36-7C10-45C4-9EF6-C284E56A8EA2.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP/files/GUID-0C8F8E36-7C10-45C4-9EF6-C284E56A8EA2.htm)
- Topic Type: concept
- Subtypes: autolisp
- Audience: programmer
- Experience Level: general
- Applies To: OARX / 2025
- Collection: Developer / AutoCAD-AutoLISP
- Timeline: Created 21/10/2024, Updated 02/12/2019

The blackboard namespace is not attached to any document or VLX application. You can set and reference variables in the blackboard from any document or VLX application. Use the `vl-bb-set`  function to set a variable, and use `vl-bb-ref`  to retrieve a variable's value.

For example, the following sets the foobar variable to a string in the blackboard namespace:

```lisp
(vl-bb-set 'foobar "Root toot toot")

"Root toot toot"
```

The `vl-bb-ref`  function returns the specified string. The following uses the `vl-bb-ref`  function to retrieve the value of the `foobar`  variable from the blackboard namespace:

```lisp
(vl-bb-ref 'foobar)

"Root toot toot"
```

Setting or retrieving variable values in the blackboard namespace has no effect on variables of the same name in any other namespace.
