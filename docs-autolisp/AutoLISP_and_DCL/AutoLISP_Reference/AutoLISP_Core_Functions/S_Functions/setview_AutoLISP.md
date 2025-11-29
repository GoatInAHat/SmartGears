---
title: setview (AutoLISP)
guid: "GUID-EEABEF66-423A-4D2B-BE16-AA6A56B08FBD"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EEABEF66-423A-4D2B-BE16-AA6A56B08FBD.htm"
generated: "2025-11-28T19:06:41.114074Z"
description: Establishes a view for a specified viewport
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

# setview (AutoLISP)

> Establishes a view for a specified viewport

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EEABEF66-423A-4D2B-BE16-AA6A56B08FBD.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-EEABEF66-423A-4D2B-BE16-AA6A56B08FBD.htm)
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
(setview
view_descriptor [vport_id]
)
```

- ***view_descriptor*:** **Type:**  Ename (entity name)  An entity definition list similar to that returned by `tblsearch`  when applied to the VIEW symbol table.
- ***vport_id*:** **Type:**  Integer  Viewport to receive the new view. If *vport_id*  is 0, the current viewport receives the new view.  You can obtain the *vport_id*  number from the AutoCAD CVPORT system variable.

## Return Values

**Type:**  Ename (entity name) or nil

If successful, the `setview`  function returns the *view_descriptor*.

## Examples

N/A
