---
title: redraw (AutoLISP)
guid: "GUID-4A4DCECD-E85A-4860-A58F-56B48227857F"
source_url: "https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4A4DCECD-E85A-4860-A58F-56B48227857F.htm"
generated: "2025-11-28T19:06:39.653958Z"
description: Redraws the current viewport or a specified object (entity) in the current viewport
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

# redraw (AutoLISP)

> Redraws the current viewport or a specified object (entity) in the current viewport

## Quick Reference

- Source: [https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4A4DCECD-E85A-4860-A58F-56B48227857F.htm](https://help.autodesk.com/cloudhelp/2025/ENU/AutoCAD-AutoLISP-Reference/files/GUID-4A4DCECD-E85A-4860-A58F-56B48227857F.htm)
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
(redraw
[ename [mode]]
)
```

- ***ename*:** **Type:**  ads_name  The name of the entity name to be redrawn.
- ***mode*:** **Type:**  Integer  Value that controls the visibility and highlighting of the entity. The *mode*  can be one of the following values:  **1**  -- Show entity  **2**  -- Hide entity (blank it out)  **3**  -- Highlight entity  **4**  -- Unhighlight entity  The use of entity highlighting (mode 3) must be balanced with entity unhighlighting (mode 4).

## Return Values

**Type:**  nil

Always returns `nil`.

## Remarks

If `redraw`  is called with no arguments, the function redraws the current viewport. If called with an entity name argument, `redraw`  redraws the specified entity.

The `redraw`  function has no effect on highlighted or hidden entities; however, a AutoCAD REGEN command forces the entities to redisplay in their normal manner.

If *ename*  is the header of a complex entity (a polyline or a block reference with attributes), `redraw`  processes the main entity and all its subentities if the *mode*  argument is positive. If the *mode*  argument is negative, `redraw`  operates on only the header entity.

## Examples

None
